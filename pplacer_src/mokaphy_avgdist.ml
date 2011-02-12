
open MapsSets
open Fam_batteries

module Prefs = struct
  type mokaphy_prefs = 
    {
      use_pp: bool ref;
      out_fname: string ref;
      list_output: bool ref;
      weighted: bool ref;
      exponent: float ref;
    }
  
  let use_pp            p = !(p.use_pp)
  let out_fname         p = !(p.out_fname)
  let list_output       p = !(p.list_output)
  let weighted          p = !(p.weighted)
  let exponent          p = !(p.exponent)
  
  let defaults () = 
    { 
      use_pp = ref false;
      out_fname = ref "";
      list_output = ref false;
      weighted = ref false;
      exponent = ref 1.;
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_fname,
    "Set the filename to write to. Otherwise write to stdout.";
    "-p", Arg.Set prefs.use_pp,
    "Use posterior probability.";
    "--list-out", Arg.Set prefs.list_output,
    "Output the avgdist results as a list rather than a matrix.";
    "--unweighted", Arg.Clear prefs.weighted,
    Mokaphy_prefs.weighted_help;
    "--exp", Arg.Set_float prefs.exponent,
    "An exponent applied before summation of distances.";
    ]
end 


type 'a data_t = 
  | Pairwise of 'a Placerun.placerun * 'a Placerun.placerun 
  | Single of 'a Placerun.placerun

let of_placerun_gen dist_fun data = 
  let total = ref 0. in
  let add_to_tot tl a b = total := !total +. (dist_fun a b) /. tl in
  match data with
  | Single pr ->
      let tl = Gtree.tree_length (Placerun.get_ref_tree pr) 
      and pql = Placerun.get_pqueries pr in
      Base.list_iter_over_pairs_of_single (add_to_tot tl) pql;
      (* now do diagonal *)
      List.iter (fun pq -> add_to_tot tl pq pq) pql;
      let n = List.length pql in
      (!total) /. (float_of_int ((n*(n+1))/2))
  | Pairwise (pr, pr') ->
      let tl = Gtree.tree_length (Placerun.get_same_tree pr pr') in
      Base.list_iter_over_pairs_of_two 
        (add_to_tot tl)
        (Placerun.get_pqueries pr)
        (Placerun.get_pqueries pr');
      (!total) /. 
        (float_of_int ((Placerun.n_pqueries pr)*(Placerun.n_pqueries pr')))

let of_placerun_pair dist_fun pr pr' = 
  of_placerun_gen dist_fun (Pairwise (pr,pr')) 

let of_placerun dist_fun pr = 
  of_placerun_gen dist_fun (Single pr) 

let make_dist_fun prefs prl = 
  Pquery_distances.dist_fun_of_expon_weight 
    (Prefs.exponent prefs)
    (Mokaphy_prefs.weighting_of_bool (Prefs.weighted prefs))
    (Mokaphy_prefs.criterion_of_bool (Prefs.use_pp prefs))
    (Edge_rdist.build_ca_info (Cmds_common.list_get_same_tree prl))

let uavgdist prefs prl = 
  Cmds_common.wrap_output 
    (Prefs.out_fname prefs) 
    (Cmds_common.write_unary
      (of_placerun 
        (make_dist_fun prefs prl))
      prl)

let bavgdist prefs prl = 
  let pra = Array.of_list prl in
  Cmds_common.wrap_output 
    (Prefs.out_fname prefs) 
    (Cmds_common.write_uptri
      (Prefs.list_output prefs)
      (Array.map Placerun.get_name pra)
      "bavgdst"
      (Uptri.init
        (Array.length pra)
        (fun i j -> 
          of_placerun_pair 
            (make_dist_fun prefs prl)
            pra.(i) 
            pra.(j))))


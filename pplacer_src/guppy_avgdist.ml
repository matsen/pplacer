open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

let write_unary pr_to_float prl ch =
  String_matrix.write_padded
   ch
   (Array.map
     (fun pr ->
       [|
         Placerun.get_name pr;
         Printf.sprintf "%g" (pr_to_float pr);
       |])
   (Array.of_list prl))

let write_uptri list_output namea fun_name u ch =
  if Uptri.get_dim u = 0 then
    failwith(Printf.sprintf "can't do %s with fewer than two place files" fun_name);
  if list_output then begin
    String_matrix.write_padded ch
      (Array.of_list
        ([|"sample_1"; "sample_2"; fun_name;|]::
          (let m = ref [] in
          Uptri.iterij
            (fun i j s -> m := [|namea.(i); namea.(j); s|]::!m)
            (Uptri.map (Printf.sprintf "%g") u);
          List.rev !m)))
  end
  else begin
    Printf.fprintf ch "%s distances:\n" fun_name;
    Mokaphy_common.write_named_float_uptri ch namea u;
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

class virtual base_cmd () =
object (self)
  inherit subcommand () as super
  inherit outfile_cmd () as super_outfile
  inherit mass_cmd () as super_mass
  inherit kr_cmd () as super_kr
  inherit placefile_cmd () as super_placefile

  val list_output = flag "--list-out"
    (Plain (false, "Output the avgdist results as a list rather than a matrix."))

  method specl =
    super_outfile#specl
    @ super_mass#specl
    @ super_kr#specl
    @ [
      toggle_flag list_output;
    ]

  method private make_dist_fun prl =
    let _, weighting, criterion = self#mass_opts in
    Pquery_distances.dist_fun_of_expon_weight
      (fv p_exp)
      weighting
      criterion
      (Edge_rdist.build_ca_info (Mokaphy_common.list_get_same_tree prl))
end

class bavgdist_cmd () =
object (self)
  inherit base_cmd ()

  method desc =
"calculates the average pairwise distance between place files"
  method usage = "usage: bavgdist [options] placefiles"

  method private placefile_action prl =
    let pra = Array.of_list prl in
    write_uptri
      (fv list_output)
      (Array.map Placerun.get_name pra)
      "bavgdst"
      (Uptri.init
         (Array.length pra)
         (fun i j ->
           of_placerun_pair
             (self#make_dist_fun prl)
             pra.(i)
             pra.(j)))
      self#out_channel
end

class uavgdist_cmd () =
object (self)
  inherit base_cmd ()

  method desc =
"calculates the average pairwise distance within place files"
  method usage = "usage: uavgdist [options] placefile(s)"

  method private placefile_action prl =
    write_unary
      (of_placerun (self#make_dist_fun prl))
      prl
      self#out_channel
end

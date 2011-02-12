open Fam_batteries
open MapsSets

module Prefs = struct
  type mokaphy_prefs = 
    {
      use_pp: bool ref;
      transform : string ref;
      verbose: bool ref;
      n_samples: int ref;
      out_fname: string ref;
      list_output: bool ref;
      density: bool ref;
      p_plot: bool ref;
      box_plot: bool ref;
      p_exp: float ref;
      weighted: bool ref;
      seed: int ref;
      bary_density: bool ref;
      ddensity: bool ref;
      refpkg_path : string ref;
    }
  
  let use_pp            p = !(p.use_pp)
  let transform         p = !(p.transform)
  let verbose           p = !(p.verbose)
  let n_samples         p = !(p.n_samples)
  let out_fname         p = !(p.out_fname)
  let list_output       p = !(p.list_output)
  let density           p = !(p.density)
  let p_plot            p = !(p.p_plot)
  let box_plot          p = !(p.box_plot)
  let p_exp             p = !(p.p_exp)
  let weighted          p = !(p.weighted)
  let seed              p = !(p.seed)
  let bary_density      p = !(p.bary_density)
  let ddensity          p = !(p.ddensity)
  let refpkg_path       p = !(p.refpkg_path)
  
  let defaults () = 
    { 
      use_pp = ref false;
      verbose = ref false;
      n_samples = ref 0;
      out_fname = ref "";
      list_output = ref false;
      density = ref false;
      p_plot = ref false;
      box_plot = ref false;
      p_exp = ref 1.;
      weighted = ref true;
      seed = ref 1;
      bary_density = ref false;
      ddensity = ref false;
      refpkg_path = ref "";
      transform = ref "";
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_fname,
    "Set the filename to write to. Otherwise write to stdout.";
    "-c", Arg.Set_string prefs.refpkg_path,
    (Mokaphy_common.refpkg_help "kr");
    "-p", Arg.Set prefs.use_pp,
    "Use posterior probability.";
    "--exp", Arg.Set_float prefs.p_exp,
    "The exponent for the integration, i.e. the value of p in Z_p.";
    "--unweighted", Arg.Clear prefs.weighted,
    Mokaphy_common.weighted_help;
    "--transform", Arg.Set_string prefs.transform,
    Mokaphy_common.transform_help;
    "--list-out", Arg.Set prefs.list_output,
    "Output the KR results as a list rather than a matrix.";
    "--density", Arg.Set prefs.density,
    "write out a shuffle density data file for each pair.";
(*
    "--pplot", Arg.Set prefs.p_plot,
        "write out a plot of the distances when varying the p for the Z_p calculation";
    "--box", Arg.Set prefs.box_plot,
        "write out a box and point plot showing the original sample distances compared to the shuffled ones.";
*)
    "-s", Arg.Set_int prefs.n_samples,
        ("Set how many samples to use for significance calculation (0 means \
        calculate distance only). Default is "^(string_of_int (n_samples prefs)));
    Mokaphy_common.spec_with_default "--seed" (fun o -> Arg.Set_int o) prefs.seed
    "Set the random seed, an integer > 0. Default is %d.";
    "--verbose", Arg.Set prefs.verbose,
    "Verbose running.";
]
end 


type result = 
  {
    distance : float;
    p_value : float option;
  }

let get_distance r = r.distance
let get_p_value r = match r.p_value with
  | Some p -> p
  | None -> failwith "no p-value!"

let make_shuffled_pres transform n_shuffles pre1 pre2 = 
  let pre_arr = Array.of_list (pre1 @ pre2)
  and n1 = List.length pre1
  and n2 = List.length pre2
  in
  let pquery_sub start len = 
    Mass_map.Pre.normalize_mass transform
      (Array.to_list (Array.sub pre_arr start len)) 
  in
  ListFuns.init 
    n_shuffles
    (fun _ ->
      Mokaphy_base.shuffle pre_arr;
      (pquery_sub 0 n1, pquery_sub n1 n2))

let pair_core transform p n_samples t pre1 pre2 =
  let calc_dist = Kr_distance.scaled_dist_of_pres transform p t in
  let original_dist = calc_dist pre1 pre2 in
  {
    distance = original_dist; 
    p_value = 
      if 0 < n_samples then begin
        let shuffled_dists = 
          List.map 
            (fun (spre1,spre2) -> calc_dist spre1 spre2)
            (make_shuffled_pres transform n_samples pre1 pre2)
        in
        Some
          (Mokaphy_base.list_onesided_pvalue 
            shuffled_dists 
            original_dist)
      end
      else None;
  }

  
(* core
 * run pair_core for each unique pair 
 *)
let core ch prefs prl = 
  if List.length prl = 0 then 
    exit 0; (* zero mokaphy --help *)
  if List.length prl = 1 then 
    invalid_arg "can't do KR with fewer than two place files";
  let n_samples = Prefs.n_samples prefs 
  and is_weighted = Prefs.weighted prefs
  and use_pp = Prefs.use_pp prefs
  and pra = Array.of_list prl 
  and p = Prefs.p_exp prefs
  and transform = Mass_map.transform_of_str (Prefs.transform prefs)
  and tax_refpkgo = match Prefs.refpkg_path prefs with
      | "" -> None
      | path -> 
        let rp = Refpkg.of_path path in
        if Refpkg.tax_equipped rp then Some rp
        else None
  in
  (* below is for make_shuffled_pres *)
  Random.init (Prefs.seed prefs);
  (* in the next section, pre_f is a function which takes a pr and makes a pre,
   * and t is a gtree *)
  let uptri_of_t_pre_f (t, pre_f) = 
    let prea = Array.map pre_f pra in
    Uptri.init
      (Array.length prea)
      (fun i j ->
        let context = 
          Printf.sprintf "comparing %s with %s" 
            (Placerun.get_name pra.(i)) (Placerun.get_name pra.(j))
        in
        try pair_core transform p n_samples t prea.(i) prea.(j) with
        | Kr_distance.Invalid_place_loc a -> 
            invalid_arg
            (Printf.sprintf 
               "%g is not a valid placement location when %s" a context)
        | Kr_distance.Total_kr_not_zero tkr ->
            failwith 
               ("total kr_vect not zero for "^context^": "^
                  (string_of_float tkr)))
  (* here we make one of these pairs from a function which tells us how to
   * assign a branch length to a tax rank *)
  and t_pre_f_of_bl_of_rank rp bl_of_rank = 
    let (taxt, ti_imap) = Tax_gtree.of_refpkg_gen bl_of_rank rp in
    (Decor_gtree.to_newick_gtree taxt, 
    Mokaphy_common.make_tax_pre taxt ~is_weighted ~use_pp ti_imap)
  in
  (* here we make a list of uptris, which are to get printed *)
  let uptris =
    List.map 
      uptri_of_t_pre_f
      ([Mokaphy_common.list_get_same_tree prl, 
      Mokaphy_common.pre_of_pr ~is_weighted ~use_pp] @
      (match tax_refpkgo with
      | None -> []
      | Some rp -> 
          List.map (t_pre_f_of_bl_of_rank rp)
                   [Tax_gtree.unit_bl; Tax_gtree.inverse]))
  (* here are a list of function names to go with those uptris *)
  and fun_names = 
    List.map 
      (fun s -> Printf.sprintf "%s%g" s p)
      (["Z_"] @ 
      (match tax_refpkgo with 
      | Some _ -> ["unit_tax_Z_"; "inv_tax_Z_"] 
      | None -> []))
  (* the names of the placeruns *)
  and names = Array.map Placerun.get_name pra
  and print_pvalues = n_samples > 0
  and neighborly f l = List.flatten (List.map f l)
  in
  Mokaphy_common.write_uptril 
    (Prefs.list_output prefs)
    names
    (if print_pvalues then neighborly (fun s -> [s;s^"_p_value"]) fun_names 
    else fun_names)
    (if print_pvalues then 
      neighborly (fun u -> [Uptri.map get_distance u; Uptri.map get_p_value u]) uptris 
    else (List.map (Uptri.map get_distance) uptris))
    ch


let kr prefs prl = 
  Mokaphy_common.wrap_output (Prefs.out_fname prefs)
    (fun ch -> core ch prefs prl)


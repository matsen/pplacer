
module Prefs = struct
  type mokaphy_prefs = 
    {
      out_prefix: string ref;
      use_pp: bool ref;
      weighted: bool ref;
      write_n: int ref;
      refpkg_path : string ref;
      scale: bool ref;
      multiplier: float ref;
      transform: string ref;
    }
  
  let out_prefix  p = !(p.out_prefix)
  let use_pp      p = !(p.use_pp)
  let weighted    p = !(p.weighted)
  let write_n     p = !(p.write_n)
  let refpkg_path p = !(p.refpkg_path)
  let scale       p = !(p.scale)
  let multiplier  p = !(p.multiplier)
  let transform   p = !(p.transform)
  
  let defaults () = 
    { 
      out_prefix = ref "";
      use_pp = ref false;
      weighted = ref false;
      write_n = ref 5;
      refpkg_path = ref "";
      scale = ref false;
      multiplier = ref 50.;
      transform = ref "";
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_prefix,
    "Specify an out prefix.";
    "-p", Arg.Set prefs.use_pp,
    "Use posterior probability.";
    "-c", Arg.Set_string prefs.refpkg_path,
    (Mokaphy_common.refpkg_help "cluster");
    "--unweighted", Arg.Clear prefs.weighted,
    Mokaphy_common.weighted_help;
    "--write-n", Arg.Set_int prefs.write_n,
    "The number of principal coordinates to write out (default is 5).";
    "--scale", Arg.Set prefs.scale,
    "Scale variances to one before performing principal components.";
    "--multiplier", Arg.Set_float prefs.multiplier,
    "The factor by which we multiply the principal component eigenvectors to get branch thickness.";
    "--transform", Arg.Set_string prefs.transform,
    Mokaphy_common.transform_help;
    ]
end 



open MapsSets
open Fam_batteries

let tolerance = 1e-3

(* *** splitify *** *)

let splitify x = x -. (1. -. x)

let soft_find i m = if IntMap.mem i m then IntMap.find i m else 0.

let arr_of_map len m = Array.init len (fun i -> soft_find i m)

let map_of_arr a = 
  let m = ref IntMap.empty in
  Array.iteri (fun i x -> m := IntMap.add i x (!m)) a;
  !m

let map_filter f m = 
  IntMap.fold
    (fun k v m -> if f k v then IntMap.add k v m else m)
    m
    IntMap.empty

(* get the mass below the given edge, excluding that edge *)
let below_mass_map edgem t = 
  let m = ref IntMap.empty in
  let total = 
    Gtree.recur
      (fun i below_massl -> 
        let below_tot = List.fold_left ( +. ) 0. below_massl in
        m := IntMapFuns.check_add i below_tot (!m);
        (soft_find i edgem) +. below_tot)
      (fun i -> soft_find i edgem)
      t
  in 
  assert(abs_float(1. -. total) < tolerance);
  !m

(* Take a placerun and turn it into a vector which is indexed by the edges of
 * the tree.
 * Later we may cut the edge mass in half; right now we don't do anything with it. *)
let splitify_placerun transform weighting criterion pr = 
  let preim = Mass_map.Pre.of_placerun weighting criterion pr
  and t = Placerun.get_ref_tree pr
  in 
  arr_of_map 
    (1+(Gtree.top_id t))
    (IntMap.map 
      splitify 
      (below_mass_map (Mass_map.By_edge.of_pre transform preim) t))

let heat_map_of_floatim multiplier m = 
  let min_width = 1. in 
  IntMap.map
    (fun v ->
      if v = 0. then []
      else begin
        let width = multiplier *. (abs_float v) in
        (Heat_tree.simple_color_of_heat v)::
          (if width < min_width then []
           else [Decor.width width])
      end)
    m

let heat_tree_of_floatim multiplier t m =
  Placeviz_core.spread_short_fat 1e-2
    (Decor_gtree.add_decor_by_map t ((heat_map_of_floatim multiplier) m))

let save_named_fal fname nvl = 
  Csv.save 
    fname
    (List.map
      (fun (name, v) -> name::(List.map string_of_float (Array.to_list v)))
      nvl)

let pca_complete ?scale transform
      weighting criterion multiplier write_n refpkgo out_prefix prl = 
  let prt = Mokaphy_common.list_get_same_tree prl in
  let t = match refpkgo with 
  | None -> Decor_gtree.of_newick_gtree prt
  | Some rp -> 
      Mokaphy_common.check_refpkgo_tree prt refpkgo;
      Refpkg.get_tax_ref_tree rp
  in
  let data = List.map (splitify_placerun transform weighting criterion) prl
  in
  let (eval, evect) = Pca.gen_pca ?scale ~n_keep:write_n (Array.of_list data)
  in
  let combol = (List.combine (Array.to_list eval) (Array.to_list evect))
  and names = (List.map Placerun.get_name prl)
  in
  Phyloxml.named_gtrees_to_file
    (out_prefix^".xml")
    (List.map
      (fun (eval, evect) ->
        (Some (string_of_float eval),
        heat_tree_of_floatim multiplier t (map_of_arr evect)))
      combol);
  save_named_fal
    (out_prefix^".rot")
    (List.map (fun (eval, evect) -> (string_of_float eval, evect)) combol);
  save_named_fal
    (out_prefix^".trans")
    (List.combine 
      names 
      (List.map (fun d -> Array.map (Pca.dot d) evect) data));
  save_named_fal
    (out_prefix^".edgediff")
    (List.combine names data);
  ()


let pca prefs = function
  | [] -> ()
  | prl ->
      match Prefs.out_prefix prefs with 
      | "" -> failwith "Please specify an out prefix for pca with -o"
      | prefix ->
      pca_complete 
        ~scale:(Prefs.scale prefs)
        (Mass_map.transform_of_str (Prefs.transform prefs))
        (Mokaphy_common.weighting_of_bool (Prefs.weighted prefs))
        (Mokaphy_common.criterion_of_bool (Prefs.use_pp prefs))
        (Prefs.multiplier prefs)
        (Prefs.write_n prefs)
        (Mokaphy_common.refpkgo_of_fname (Prefs.refpkg_path prefs))
        prefix
        prl

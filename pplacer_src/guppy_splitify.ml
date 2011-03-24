open Subcommand
open Guppy_cmdobjs

open MapsSets
open Fam_batteries

let tolerance = 1e-3

(* *** splitify *** *)

let splitify x = x -. (1. -. x)

let soft_find i m = if IntMap.mem i m then IntMap.find i m else 0.

let arr_of_map len m = Array.init len (fun i -> soft_find i m)

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

let save_named_fal fname nvl =
  Csv.save
    fname
    (List.map
      (fun (name, v) -> name::(List.map string_of_float (Array.to_list v)))
      nvl)


class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile

  method specl =
    super_out_prefix#specl
    @ super_mass#specl

  method desc =
"write out splits of masses"
  method usage = "usage: splitify [options] placefile"

  method private placefile_action prl =
    let transform, weighting, criterion = self#mass_opts in
    self#check_placerunl prl;
    let data = List.map (splitify_placerun transform weighting criterion) prl
    and names = (List.map Placerun.get_name prl)
    in
    save_named_fal
      (out_prefix^".edgediff")
      (List.combine names data)
end

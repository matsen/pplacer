open Subcommand
open Guppy_cmdobjs

open Ppatteries

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
        m := IntMap.check_add i below_tot (!m);
        (soft_find i edgem) +. below_tot)
      (fun i -> soft_find i edgem)
      t
  in
  assert(abs_float(1. -. total) < tolerance);
  !m

(* Take a placerun and turn it into a vector which is indexed by the edges of
 * the tree.
 * Later we may cut the edge mass in half; right now we don't do anything with it. *)
let splitify_placerun weighting criterion pr =
  let preim = Mass_map.Pre.of_placerun weighting criterion pr
  and t = Placerun.get_ref_tree pr
  in
  arr_of_map
    (1+(Gtree.top_id t))
    (IntMap.map
      splitify
      (below_mass_map (Mass_map.By_edge.of_pre preim) t))

let fal_to_strll fal =
  List.map
    (fun (name, v) -> name::(List.map string_of_float (Array.to_list v)))
    fal

let save_out_named_fal ch fal =
  csv_out_channel ch
    |> Csv.to_out_obj
    |> flip Csv.output_all (fal_to_strll fal)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile

  method specl =
    super_output#specl
    @ super_mass#specl

  method desc =
"writes out differences of masses for the splits of the tree"
  method usage = "usage: splitify [options] placefile(s)"

  method private placefile_action prl =
    let weighting, criterion = self#mass_opts in
    let data = List.map (splitify_placerun weighting criterion) prl
    and names = (List.map Placerun.get_name prl)
    in
    save_out_named_fal
      self#out_channel
      (List.combine names data)
end

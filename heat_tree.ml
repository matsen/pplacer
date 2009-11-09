(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets
open Fam_batteries

(* settings *)

let gray = 5 (* the strength of gray *)
let min_width = 0.5
let max_width = 25.5


(* color utils *)

(* intesity is a float from 0 to 1 which is the absolute-valued and
 * exponentiated version of the heat *)
let intensity_of_heat ~p heat = (abs_float heat) ** p

let assert_intensity intensity = 
  assert(intensity >= 0. || intensity <= 1.)

let fgray = float_of_int gray
let gray_scale = 255. -. fgray

let color_ubyte_of_intensity intensity = 
  assert_intensity intensity;
  int_of_float(fgray +. intensity *. gray_scale)

let color_of_heat ?(p=1.) heat = 
  let uheat = 
    color_ubyte_of_intensity (intensity_of_heat ~p heat) in
  if heat >= 0. then Decor.gray_red ~gray_level:gray uheat
  else Decor.gray_blue ~gray_level:gray uheat

(* width utils *)
let width_diff = max_width -. min_width

let width_of_heat ?(p=1.) heat = 
  let intensity = intensity_of_heat ~p heat in
  assert_intensity intensity;
  Decor.width (min_width +. width_diff *. intensity)

let color_map_aux weighting criterion p pr1 pr2 = 
  let ref_tree = Placerun.get_same_tree pr1 pr2
  and kr_map = 
    IntMap.map
    (* we don't care about where we are along the edge *)
      (List.map snd) 
      (Kr_distance.make_kr_map 
        (Mass_map.Indiv.of_placerun weighting criterion pr1)
        (Mass_map.Indiv.of_placerun weighting criterion pr2)) in
  let sum_over_krs_of_id id = 
    List.fold_right
      (fun kr_v -> ( +. ) (kr_v.(0) -. kr_v.(1)))
      (Base.get_from_list_intmap id kr_map)
  in
  let heat_list = 
    Stree.recur_listly
      (fun id below ->
        ((id,
          sum_over_krs_of_id 
            id 
(* the first item a list from a subtree is the total of all of the heat in that
* subtree. therefore to get the total heat for our tree, we just have to total
* all of those *)
            (List.fold_left 
              (fun accu -> function
                | (_,heat)::_ -> heat +. accu
                | [] -> accu)
              0.
              below)))
        :: (List.flatten below))
      (Gtree.get_stree ref_tree)
  in
  let heat_only = List.map snd heat_list in
  let top_heat = List.hd heat_only in
  if top_heat > Kr_distance.tol then
    raise (Kr_distance.Total_kr_not_zero top_heat);
  let max_abs_heat = 
    max
      (ListFuns.complete_fold_left max heat_only)
      (-. (ListFuns.complete_fold_left min heat_only))
  in
  IntMapFuns.of_pairlist
    (List.map 
      (fun (id, raw_heat) -> 
        let scaled_heat = raw_heat /. max_abs_heat in
        (id, 
          [
            color_of_heat ~p scaled_heat;
            width_of_heat ~p scaled_heat;
          ]))
      heat_list) 

let color_map rev_video weighting criterion p pr1 pr2 = 
  let result = 
    color_map_aux weighting criterion p pr1 pr2
  in
  if rev_video = false then result
  else (IntMap.map (List.map Decor.rev_color) result)

let make_heat_tree rev_video criterion weighting p pr1 pr2 = 
  let ref_tree = 
    Placerun.get_same 
      Newick.compare 
      Placerun.get_ref_tree 
      "Reference tree" 
      pr1 pr2
  in
  Decor_gtree.add_decor_by_map 
    (Decor_gtree.of_newick_gtree ref_tree)
    (color_map rev_video criterion weighting p pr1 pr2)

let write_heat_tree rev_video criterion weighting p pr1 pr2 =
  Phyloxml.tree_to_file
    (make_heat_tree rev_video criterion weighting p pr1 pr2)
    (Printf.sprintf "%s.VS.%s.heat.xml"
      (Placerun.get_name pr1)
      (Placerun.get_name pr2))

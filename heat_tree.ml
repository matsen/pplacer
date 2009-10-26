(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets
open Fam_batteries


(* color utils *)
let unsigned_byte_of_heat heat = 
  assert(heat >= 0. || heat <= 1.);
  int_of_float(heat *. 255.)

let color_of_heat heat = 
  let uheat = unsigned_byte_of_heat (abs_float heat) in
  let rev_uheat = 255 - uheat in
  if heat >= 0. then
    Ftree.Color(255, rev_uheat, rev_uheat)
  else
    Ftree.Color(rev_uheat, rev_uheat, 255)

(* width utils *)
let min_width = 0.5
let max_width = 15.
let width_diff = max_width -. min_width

let width_of_heat heat = 
  Ftree.Width(min_width +. width_diff *. heat)

let color_map_aux criterion ref_tree p pcl1 pcl2 = 
  let kr_map = 
    IntMap.map
    (* we don't care about where we are along the edge *)
      (List.map snd) 
      (Kr_distance.make_kr_map criterion pcl1 pcl2) in
  let sum_over_krs_of_id id = 
    List.fold_right
      (fun kr_v -> ( +. ) (kr_v.(0) -. kr_v.(1)))
      (Base.get_from_list_intmap id kr_map)
  in
  let heat_list = 
    List.map
      (fun (id, raw_heat) -> (id, raw_heat ** p))
      (Stree.recur_listly
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
        (Itree.get_stree ref_tree))
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
            color_of_heat scaled_heat;
            width_of_heat (abs_float scaled_heat);
          ]))
      heat_list) 

let color_map criterion weighting p pr1 pr2 = 
  Placerun_distance.pair_dist_gen 
    (Placerun_distance.process_pcl weighting criterion)
    (color_map_aux criterion)
    p 
    pr1 
    pr2

let make_heat_tree criterion weighting p pr1 pr2 = 
  let ref_tree = 
    Placerun.get_same Placerun.get_ref_tree "Reference tree" pr1 pr2
  in
  Ftree.make ref_tree (color_map criterion weighting p pr1 pr2)

let write_heat_tree criterion weighting p pr1 pr2 =
  Phyloxml.tree_to_file
    (make_heat_tree criterion weighting p pr1 pr2)
    (Printf.sprintf "%s.VS.%s.heat.xml"
      (Placerun.get_name pr1)
      (Placerun.get_name pr2))

(* mokaphy v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets
open Fam_batteries

module MP = Mokaphy_prefs.Heat

(* color utils *)

(* intesity is a float from 0 to 1 which is the absolute-valued and
 * exponentiated version of the heat *)
let intensity_of_heat ~p heat = (abs_float heat) ** p

let assert_intensity intensity = 
  assert(intensity >= 0. || intensity <= 1.)

let simple_color_of_heat heat = 
  if heat >= 0. then Decor.red else Decor.blue

let gray_black_of_heat heat = 
  if heat >= 0. then Decor.gray 180 else Decor.black

let color_of_heat prefs ?(p=1.) heat = 
  let gray_level = MP.gray_level prefs in
  let intensity = intensity_of_heat ~p heat
  and color = simple_color_of_heat heat
  and gray = 
    Decor.gray 
      (if MP.white_bg prefs then 
        255-gray_level 
      else 
        gray_level)
  in
  assert_intensity intensity;
  Decor.color_avg intensity color gray

let width_value_of_heat ~width_diff ?(p=1.) heat = 
  let intensity = intensity_of_heat ~p heat in
  assert_intensity intensity;
  width_diff *. intensity

let color_map prefs t pre1 pre2 = 
  let transform = Mass_map.transform_of_str "" in
  let p = MP.p_exp prefs
  and kr_map = 
    IntMap.map
    (* we don't care about where we are along the edge *)
      (List.map snd) 
      (Kr_distance.make_kr_map 
        (Mass_map.Indiv.of_pre transform pre1)
        (Mass_map.Indiv.of_pre transform pre2)) in
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
      (Gtree.get_stree t)
  in
  let heat_only = List.map snd heat_list in
  let top_heat = List.hd heat_only in
  if top_heat > Kr_distance.tol then
    raise (Kr_distance.Total_kr_not_zero top_heat);
  (* why do I do it like this rather than mapping abs first? *)
  let max_abs_heat = 
    max
      (ListFuns.complete_fold_left max heat_only)
      (-. (ListFuns.complete_fold_left min heat_only))
  in
  let our_color_of_heat scaled_heat = 
    if MP.simple_colors prefs then 
      simple_color_of_heat scaled_heat
    else if MP.gray_black_colors prefs then 
      gray_black_of_heat scaled_heat
    else color_of_heat prefs ~p scaled_heat
  in
  let min_width = MP.min_width prefs in
  let width_diff = (MP.max_width prefs) -. min_width in
  IntMapFuns.of_pairlist
    (List.map 
      (fun (id, raw_heat) -> 
        let scaled_heat = raw_heat /. max_abs_heat in
        let wv = width_value_of_heat ~width_diff ~p scaled_heat in
        (id, 
        if wv = 0. then []
        else 
          ( our_color_of_heat scaled_heat ) ::
          ( if wv < min_width then []
            else [ Decor.width wv ])))
      heat_list)

let make_heat_tree prefs decor_t pre1 pre2 = 
  Decor_gtree.add_decor_by_map 
    decor_t
    (color_map prefs decor_t pre1 pre2)

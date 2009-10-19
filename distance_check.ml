(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Check the p = 2 KR calculation using distances.
*)

let make_pl_map placerun = 
  snd 
    (Placerun.make_map_by_best_loc 
      Placement.ml_ratio
      placerun)
 
let matrix_version pr1 pr2 = 
  let pl1 = make_pl_map pr1
  and pl2 = make_pl_map pr2 in
  let (n_p1, ndm1) = Distance_mat.make_numbered_distal_map 0 pl_map 
  let (tot_n_p, ndm2) = 
    Distance_mat.make_numbered_distal_map ndm1 pl_map 
  in
  Distance_mat.of_numbered_placement_map t n_p npm
let of_placement_list_map t pl_map =


let check pr1 pr2 = 
  let kr =
    Placerun_distance.pair_dist 
      Placement.ml_ratio
      Placerun_distance.Unweighted
      2
      pr1
      pr2
  in
    (Placerun.get_ref_tree placerun) 
    (IntMap.map 
      (List.map (Pquery.best_place Placement.ml_ratio))
      pl_map)



let ml_best_of_placerun placerun = 

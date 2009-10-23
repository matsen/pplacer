(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Check the p = 2 KR calculation using distances.
*)

open MapsSets

let int_div x y = (float_of_int x) /. (float_of_int y) 
let sq x = x *. x

(* suboptimal, but operational *)
let quad_form a x = 
  Gsl_blas.dot x (Fam_gsl_matvec.allocMatVecMul a x)

let gsl_matrix_of_uptri u = 
  Fam_gsl_matvec.matInit
    (Uptri.get_dim u)
    (Uptri.get_dim u)
    (fun i j -> 
      if i = j then 0.
      else Uptri.get_loose u i j)

let make_pl_map placerun = 
  IntMap.map
    (List.map (Pquery.best_place Placement.ml_ratio))
    (snd 
      (Placerun.make_map_by_best_loc 
        Placement.ml_ratio
        placerun))

(* make X - 1 * m / (m+n), which is
 * ratio_for_one = n / (m+n) for the first m coords
 * ratio_for_two = - m / (m+n) for the rest (n coords)
 * and - 1/2 (m+n) / (mn)
 * *)
let make_x_diff_and_coeff ~tot_n_p ~n_p1 = 
  let m = n_p1
  and n = tot_n_p - n_p1 
  in
  let ratio_for_one = int_div n (m+n) in
  let ratio_for_two = int_div (-m) (m+n) in
  ( Fam_gsl_matvec.vecInit
      tot_n_p
      (fun i -> 
        if i < n_p1 then ratio_for_one
        else ratio_for_two),
    -. 0.5 *. (sq (int_div ((m+n)) (m*n))))

let matrix_version pr1 pr2 = 
  let plm1 = make_pl_map pr1
  and plm2 = make_pl_map pr2 in
  let t = Placerun.get_ref_tree pr1 in
  assert(t = Placerun.get_ref_tree pr2);
  let (n_p1, ndm1) = 
    Distance_mat.numbered_distal_map_of_placemap 0 plm1 in
  let (tot_n_p, ndm2) = 
    Distance_mat.numbered_distal_map_of_placemap n_p1 plm2 
  in
  let ndm_all = Base.combine_list_intmaps [ndm1; ndm2] in
  let d = 
    gsl_matrix_of_uptri
      (Distance_mat.of_numbered_distal_map t tot_n_p ndm_all)
  in
  let (x_diff, coeff) = make_x_diff_and_coeff ~tot_n_p ~n_p1 in
  (coeff *. (quad_form d x_diff) /. (Itree.tree_length t), d)

let check pr1 pr2 = 
  Printf.printf 
    "checking %s and %s...\n" 
    (Placerun.get_name pr1) 
    (Placerun.get_name pr2);
  let kr =
    sq 
      (Placerun_distance.pair_dist 
        Placement.ml_ratio
        Placerun_distance.Unweighted
        2.
        pr1
        pr2)
  in
  let mat_kr = fst (matrix_version pr1 pr2) in
  Printf.printf "%g = usual squared KR_2\n" kr;
  Printf.printf "%g = matrix KR\n\n" mat_kr;
  Printf.printf "%g = difference\n\n" (kr -. mat_kr);
  ()


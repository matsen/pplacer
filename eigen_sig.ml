(* mokaphy v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * to get significance values using eigenvalues of the common ancestry matrix.
 *
 * mtilde = \int_u G_i(u) G_j(u) \lambda(du)
 *)

let build_mtilde use_pp pr1 pr2 = 
  let t = Placerun.get_same_tree pr1 pr2 in
  let both = 
    Array.of_list
    ((Placerun.get_pqueries pr1)@(Placerun.get_pqueries pr2)) in
  let n = Array.length both
  and get_weight = 
    if use_pp then Placement.post_prob else Placement.ml_ratio
  and ca_info = Camat.build_ca_info t
  in
  let mt = Gsl_matrix.create ~init:0. n n in
  (* increment mt[i][j] by x symmetrically *)
  let mt_increment i j x =
    let incr_one i j = 
      Bigarray.Array2.unsafe_set mt i j 
        ((Bigarray.Array2.unsafe_get mt i j) +. x) in
    incr_one i j;
    incr_one j i;
  in
  for i=0 to n-1 do
    for j=i+1 to n-1 do
      Base.list_iter_over_pairs_of_two 
        (fun p1 p2 ->
          mt_increment i j 
            ((get_weight p1) *. (get_weight p2) *.
              ((Camat.find_ca_dist ca_info
                (Placement.location p1, Placement.distal_bl p1)
                (Placement.location p2, Placement.distal_bl p2)))))
        (Pquery.place_list both.(i))
        (Pquery.place_list both.(j))
    done
  done;
  mt


  let x = Placerun_io.of_file
  let y = Fam_gsl_matvec.tolerance

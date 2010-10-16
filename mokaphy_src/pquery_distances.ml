(* mokaphy v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * This builds a matrix of distances between pqueries. 
 * In fact, it does the following. 
 * Say pquery x has placements x_i, with weights P(x_i).
 * Say pquery y has placements y_i, with weights P(y_i).
 * Assume we have a distance metric d on the tree.
 * In the weighted case this computes
 *   \sum_{ij} d(x_i, x_j) P(x_i) P(x_j)
 * and in the unweighted case it's simply the distance between the placements
 * with the highest P's.
 *)

module BA1 = Bigarray.Array1
module BA2 = Bigarray.Array2

(* could be made faster by improving the way the matrices are accessed *)
let of_placeruns weighting criterion pr1 pr2 = 
  let t = Placerun.get_same_tree pr1 pr2 in
  let both = 
    Array.of_list
    ((Placerun.get_pqueries pr1)@(Placerun.get_pqueries pr2)) in
  let n = Array.length both
  and ca_info = Edge_rdist.build_ca_info t
  in
  let m = Gsl_matrix.create ~init:0. n n in
  (* set m[i][j] symmetrically *)
  let m_set i j x =
    let set_one i j = BA2.unsafe_set m i j x in
    set_one i j;
    if i <> j then set_one j i
  in
  let () = match weighting with
  | Mass_map.Weighted -> 
    for i=0 to n-1 do
      for j=i to n-1 do
        let total = ref 0. in
        Base.list_iter_over_pairs_of_two 
          (fun p1 p2 ->
            total := !total +.
              ((criterion p1) *. (criterion p2) *.
                ((Edge_rdist.find_ca_dist ca_info
                  (Placement.location p1, Placement.distal_bl p1)
                  (Placement.location p2, Placement.distal_bl p2)))))
          (Pquery.place_list both.(i))
          (Pquery.place_list both.(j));
        m_set i j (!total)
      done
    done;
  | Mass_map.Unweighted -> 
    for i=0 to n-1 do
      for j=i to n-1 do
        let p1 = Pquery.best_place criterion both.(i)
        and p2 = Pquery.best_place criterion both.(j)
        in
        m_set i j 
          ((Edge_rdist.find_ca_dist ca_info
              (Placement.location p1, Placement.distal_bl p1)
              (Placement.location p2, Placement.distal_bl p2)))
      done
    done;
  in
  m

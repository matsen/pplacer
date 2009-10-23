(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * This is where we compute the bounce distance.
 *
 * "Raw" bounce distance is not normalized to tree length.
 *
*)

(* calculate quadratic form of an uptri u and a vector v *)
let qform u v = 
  assert(Uptri.get_dim u = Array.length v);
  let tot = ref 0. in
  Uptri.iterij
    (fun i j x -> tot := !tot +. 2. *. (v.(i) *. v.(j) *. x))
    u;
  !tot

let raw_bounce_of_placement_array t pa = 
  let d = Distance_mat.of_placement_array t pa in
  qform 
    d 
    (Base.arr_normalized_prob
      (Array.map (fun p -> Placement.ml_ratio p) pa))

let raw_bounce_of_placement_list t pl = 
  raw_bounce_of_placement_array t (Array.of_list pl)

let raw_bounce_of_pquery t pq = 
  raw_bounce_of_placement_list 
    t
    (Pquery.place_list pq)

let bounce_of_pquery t pq = 
  (raw_bounce_of_pquery t pq) /. (Itree.tree_length t)


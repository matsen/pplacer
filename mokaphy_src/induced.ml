(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * This is how we record the most distal placements for a placerun. 
 * We store them as a map from the edge numbers where they live to the distance
 * from their position on the edge to the distal side of that edge.
 *
 * SPEED: replace sort in the induced maker by something which just takes the
 * smallest. make smallest in fam.batteries?
 *)

open MapsSets
open Fam_batteries

(* induced subtree of placerun *)
let of_placerun criterion pr = 
  let distal_spot_map = 
    IntMap.map
      (List.sort compare)
      (List.fold_right
        (fun pq ->
          let best = Pquery.best_place criterion pq in
          IntMapFuns.add_listly
            (Placement.location best)
            (Placement.distal_bl best))
        (Placerun.get_pqueries pr)
        IntMap.empty)
  in
  let check_edge i = 
    if IntMap.mem i distal_spot_map then
      [i, List.hd (IntMap.find i distal_spot_map)]
    else 
      []
  in
  IntMapFuns.of_pairlist
    (Stree.recur
      (fun i belowl ->
          let below = List.concat belowl in
          if below = [] then check_edge i
          else below)
      check_edge
      (Gtree.get_stree (Placerun.get_ref_tree pr)))


(* intersection of two induceds. 
 * max means that we get the most proxmal placement. *)
let stree_intersect t ind1 ind2 = 
  let m = ref IntMap.empty in
  let add k v = m := IntMap.add k v !m in
  (* return if we should continue *)
  let add_and_continue j = 
    let ofj = IntMapFuns.opt_find j in
    match (ofj ind1, ofj ind2) with
    | (Some p1, Some p2) -> add j (max p1 p2); false
    | (Some p, None) -> add j p; false
    | (None, Some p) -> add j p; false
    | (None, None) -> true
  in
  let rec aux = function
    | Stree.Node(id, tL) ->
        if add_and_continue id then List.iter aux tL
    | Stree.Leaf id -> 
        let _ = add_and_continue id in ()
  in
  aux t;
  !m

let intersect t ind1 ind2 = 
  stree_intersect (Gtree.get_stree t) ind1 ind2

(* mokaphy v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * to construct a matrix showing common ancestry on an edge-by-edge basis.
 *
 * let A and B be two edges in a tree. let C be their common ancestor, and R be
 * the root of the tree. we say that A and B are "serial" if C is one of A or B,
 * and "parallel" if not.
 *
 * the idea is that we want to be able to compute the common ancestry of two
 * placements without doing a tree traversal. to do that, for each pair of edges
 * we store the distance from the root to their common ancestor, with an
 * indication if the relationship between the edges is serial or parallel. if
 * they are parallel, then the distance from the common ancestor of two
 * placements on those edge to the root is just the included value. if they are
 * serial, then we need to add in the distance from the placement to the
 * proximal side of the placement edge.
 *
 * NOTE: this function assumes that edges are numbered in a depth first manner,
 * such that edge numbering strictly increases from a leaf to the root.
*)


open Fam_batteries

type relation_dist = Parallel of float | Serial of float

let ppr_relation_dist ff = function
 | Parallel x -> Format.fprintf ff "P%g" x
 | Serial x -> Format.fprintf ff "S%g" x

let ppr_ca_uptri ff u = Uptri.ppr_uptri ppr_relation_dist ff u

(* make an uptri with (i,j) entry equal to the distance from the root to the
 * MRCA of edges numbered i and j with Parallel and Serial as above.
 *
 * note that we assume that there are no placements on the root edge, and so do
 * not have an entry in our uptri for it.
 * *)
let build_ca_uptri t = 
  let stree = Gtree.get_stree t in
  let u = Uptri.create (Stree.n_edges stree) (Parallel 0.) in
  (* set all pairs of the below with (Parallel curr_dist) *)
  let parallel_set below curr_dist = 
    Base.list_iter_over_pairs_of_single 
      (Base.list_iter_over_pairs_of_two
        (fun i j -> Uptri.set u i j (Parallel curr_dist))) 
      below
  in
  let rec aux dist_to_root = function
    | Stree.Node(id, tL) ->
        let curr_dist = dist_to_root +. Gtree.get_bl t id in
        let below = List.map (aux curr_dist) tL in
        (* note that for the below ones we want to use curr_dist, which includes
         * the length of the current edge *)
        parallel_set below curr_dist;
        let flat_below = List.flatten below in
        (* whereas in the serial case we don't want to include current edge *)
        List.iter 
          (fun i -> Uptri.set u i id (Serial dist_to_root))
          flat_below;
        id::flat_below
    | Stree.Leaf id -> [id]
  in
  (* avoid root edge *)
  match stree with 
    | Stree.Node(_, tL) -> parallel_set (List.map (aux 0.) tL) 0.; u
    | _ -> assert(false)


(* find the distance to the common ancestor given two placements *)
let find_ca ca_uptri (edge1, distal1) (edge2, distal2) = 
  assert(edge1 <> edge2);
  match Uptri.get_loose ca_uptri edge1 edge2 with
  | Parallel x ->
      (* the common ancestor is distal to both *)
      x
  | Serial x ->
  (* if edge 1 is on top, then we need to add the distal part of its placement
   * to the common ancestry *)
      x +. (if edge1 > edge2 then distal1 else distal2)


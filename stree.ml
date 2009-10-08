(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

(* stree.ml
 * We number all of the nodes and then have maps which tell us about the edges.
 * Note that the edge information is for the node directly above the node with
 * the chosen edge.
 * *)


open MapsSets

type stree = Node of int * stree list | Leaf of int

let node i tL = Node(i, tL)
let leaf i = Leaf i

let rec n_taxa = function
  | Node(_,tL) -> List.fold_left ( + ) 0 (List.map n_taxa tL)
  | Leaf(_) -> 1

let n_edges stree = 
  let rec aux = function
    | Node(_,tL) -> List.fold_left ( + ) 1 (List.map aux tL)
    | Leaf(_) -> 1
  in
  (aux stree) - 1   (* exclude root edge *)

let collect_node_numbers stree = 
  let rec aux = function
    | Node(i,tL) -> i :: (List.flatten (List.map aux tL))
    | Leaf(i) -> [i]
  in
  List.sort compare (aux stree)

let top_id = function
  | Node(i, _) -> i
  | Leaf(i) -> i

let rec max_id = function
  | Node(i, tL) -> List.fold_left max i (List.map max_id tL)
  | Leaf(i) -> i

let multifurcating_at_root = function
  | Node(_, tL) -> List.length tL > 2
  | Leaf(_) -> false

let rec plain_to_newick = function
  | Node(i, tL) -> 
      "("^(String.concat "," (List.map plain_to_newick tL))^")"^(string_of_int i)
  | Leaf i -> string_of_int i

(* increase all of the indices of the tree by "by" *)
let rec boost by = function
  | Node(i,tL) -> Node(i+by, List.map (boost by) tL)
  | Leaf(i) -> Leaf(i+by)

let recur f_node f_leaf tree = 
  let rec aux = function
  | Node(id, tL) -> f_node id (List.map aux tL)
  | Leaf id -> f_leaf id
  in
  aux tree



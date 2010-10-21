(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Our basic tree data structure without information.
 *
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

let rec node_ids_aux = function
  | Node(i,tL) -> i :: (List.flatten (List.map node_ids_aux tL))
  | Leaf(i) -> [i]

let node_ids stree = List.sort compare (node_ids_aux stree)
let nonroot_node_ids stree = 
  try List.sort compare (List.tl (node_ids_aux stree)) with
  | Failure "tl" -> invalid_arg "nonroot_node_ids"

let rec leaf_ids = function
  | Node(_,tL) -> List.flatten (List.map leaf_ids tL)
  | Leaf(i) -> [i]

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

let rec ppr ff = function
  | Node(i, tL) -> 
      Format.fprintf ff "@[(%a)@]%d" 
        (Ppr.ppr_gen_list_inners "," ppr) tL i
  | Leaf i -> Format.pp_print_int ff i

(* the maximal outdegree of nodes in the tree *)
let rec outdegree = function
  | Node(_,tL) -> 
      List.fold_left max (List.length tL) (List.map outdegree tL)
  | Leaf _ -> 0

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

(* for functions that don't treat leaves differently than a node with empty
 * leaves. *)
let recur_listly f = recur f (fun id -> f id []) 


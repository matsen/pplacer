(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

(* stree.ml
 * We number all of the nodes and then have maps which tell us about the edges.
 * Note that the edge information is for the node directly above the node with
 * the chosen edge.
 * *)


open MapsSets

type stree = Node of int * stree list | Leaf of int

type node_info = 
  {taxon : string IntMap.t;
   boot : float IntMap.t;
   bl : float IntMap.t}

type info_stree = 
  {tree : stree;
  info : node_info}

let gstring_of_float x = Printf.sprintf "%g" x


(* stree functions *)
let node i tL = Node(i, tL)
let leaf i = Leaf i

let rec n_taxa = function
  | Node(i,tL) -> List.fold_left ( + ) 0 (List.map n_taxa tL)
  | Leaf(i) -> 1

let n_edges stree = 
  let rec aux = function
    | Node(i,tL) -> List.fold_left ( + ) 1 (List.map aux tL)
    | Leaf(i) -> 1
  in
  (aux stree) - 1   (* exclude root edge *)

let collect_node_numbers stree = 
  let rec aux = function
    | Node(i,tL) -> i :: (List.flatten (List.map aux tL))
    | Leaf(i) -> [i]
  in
  List.sort compare (aux stree)

let top_id = function
  | Node(i, tL) -> i
  | Leaf(i) -> i

let rec max_id = function
  | Node(i, tL) -> List.fold_left max i (List.map max_id tL)
  | Leaf(i) -> i

let multifurcating_at_root = function
  | Node(i, tL) -> List.length tL > 2
  | Leaf(i) -> false


(* nodeInfo functions *)

let emptyInfo = 
  {taxon = IntMap.empty;
   boot = IntMap.empty;
   bl = IntMap.empty}

let get_something entrymap entryname info id = 
  try IntMap.find id (entrymap info) with
  | Not_found -> 
      invalid_arg (Printf.sprintf "%s %d not found" entryname id)

let info_get_taxon = get_something (fun info -> info.taxon) "taxon"
let info_get_boot = get_something (fun info -> info.boot) "bootstrap"
let info_get_bl = get_something (fun info -> info.bl) "branch length"

let addInfo vertNum taxonOpt bootOpt blOpt prevInfo = 
  {taxon = IntMapFuns.opt_add vertNum taxonOpt prevInfo.taxon;
  boot = IntMapFuns.opt_add vertNum bootOpt prevInfo.boot;
  bl = IntMapFuns.opt_add vertNum blOpt prevInfo.bl}

let combineNodeInfos n1 n2 = 
  {taxon = IntMapFuns.union n1.taxon n2.taxon;
   boot = IntMapFuns.union n1.boot n2.boot;
   bl = IntMapFuns.union n1.bl n2.bl}


(* info_stree functions *)

let inform_stree t info = {tree = t; info = info}

let get_an_info get_what tree = 
  get_what tree.info

let get_taxon = get_an_info info_get_taxon
let get_boot = get_an_info info_get_boot
let get_bl = get_an_info info_get_bl

let tree_length tree = 
  let rec aux = function
    | Node(id, tL) ->
        List.fold_left 
          ( +. ) 
          (IntMap.find id tree.info.bl)
          (List.map aux tL)
    | Leaf id -> IntMap.find id tree.info.bl
  in
  match tree.tree with 
  | Node(id, tL) ->
      (* exclude the root edge *)
      List.fold_left ( +. ) 0. (List.map aux tL)
  | Leaf id -> 0.


(* make the bootstrap value into the node number *)
let make_boot_node_num tree = 
  {tree with info = 
    {tree.info with boot = 
      List.fold_right
        (fun n -> IntMap.add n (float_of_int n))
        (collect_node_numbers tree.tree)
        IntMap.empty}}


let recur f_node f_leaf tree = 
  let rec aux = function
  | Node(id, tL) -> f_node id (List.map aux tL)
  | Leaf id -> f_leaf id
  in
  aux tree.tree


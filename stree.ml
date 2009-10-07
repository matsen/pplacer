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
let rec boost_stree by = function
  | Node(i,tL) -> Node(i+by, List.map (boost_stree by) tL)
  | Leaf(i) -> Leaf(i+by)


(* nodeInfo functions *)

let emptyInfo = 
  {taxon = IntMap.empty;
   boot = IntMap.empty;
   bl = IntMap.empty}

let get_something_opt entrymap info id = 
  try Some (IntMap.find id (entrymap info)) with
  | Not_found -> None

let info_get_taxon_opt = get_something_opt (fun info -> info.taxon)
let info_get_boot_opt = get_something_opt (fun info -> info.boot)
let info_get_bl_opt = get_something_opt (fun info -> info.bl)

let get_something get_it entryname info id = 
  match get_it info id with
  | Some x -> x
  | None -> 
      invalid_arg (Printf.sprintf "%s %d not found" entryname id)

let info_get_taxon = get_something info_get_taxon_opt "taxon"
let info_get_boot = get_something info_get_boot_opt "bootstrap"
let info_get_bl = get_something info_get_bl_opt "branch length"

(* here, taxon, boot, and bl must be specified as 'a options. *)
let add_info vert_num ~taxon ~boot ~bl prev_info = 
  {taxon = IntMapFuns.opt_add vert_num taxon prev_info.taxon;
  boot = IntMapFuns.opt_add vert_num boot prev_info.boot;
  bl = IntMapFuns.opt_add vert_num bl prev_info.bl}

(* here, they don't have to be specified, but when they are they should be 'a *)
let opt_add_info vert_num ?taxon ?boot ?bl prev_info = 
  add_info vert_num ~taxon ~boot ~bl prev_info

let combine_node_infos n1 n2 = 
  {taxon = IntMapFuns.union n1.taxon n2.taxon;
   boot = IntMapFuns.union n1.boot n2.boot;
   bl = IntMapFuns.union n1.bl n2.bl}

(* increase all of the keys in the info map by "by" *)
let boost_info by info = 
  let boost_map m = 
    IntMap.fold (fun k v -> IntMap.add (k+by) v) m IntMap.empty in
  { taxon = boost_map info.taxon;
  boot = boost_map info.boot;
  bl = boost_map info.bl }

let copy_info ~dest ~src id = 
  add_info 
    id 
    ~taxon:(info_get_taxon_opt src id)
    ~boot:(info_get_boot_opt src id)
    ~bl:(info_get_bl_opt src id)
    dest


(* info_stree functions *)

let inform_stree t info = {tree = t; info = info}

let get_an_info get_what tree = 
  get_what tree.info

let get_tree ist = ist.tree
let get_info ist = ist.info
let get_taxon = get_an_info info_get_taxon
let get_boot = get_an_info info_get_boot
let get_bl = get_an_info info_get_bl

(* copy the info from src at id over to dest *)
let istree_copy_info ~dest ~src id = 
  inform_stree 
    (get_tree dest)
    (copy_info ~dest:(get_info dest) ~src:(get_info src) id)

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
  | Node(_, tL) ->
      (* exclude the root edge *)
      List.fold_left ( +. ) 0. (List.map aux tL)
  | Leaf _ -> 0.

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

(* join a list of info_trees *)
let info_join new_id tL = 
  {tree = Node(new_id, List.map get_tree tL);
  info = 
    Base.complete_fold_left
      combine_node_infos 
      (List.map get_info tL)}

let boost by ist = 
  {tree = boost_stree by ist.tree;
   info = boost_info by ist.info; }

(* join two trees and add a branch length *)
let bl_join2 t1 t2 new_id bl = 
  let without_bl = info_join new_id [t1; t2] in
  { without_bl with
  info = opt_add_info ~bl new_id without_bl.info}

(* add a subtree above the prev one, giving
 *     
 *                     |
 *                     | - - new_t
 * root bl = orig bl   |
 *                     | bl = where
 *                     |
 *                     t
 * 
 * we boost all of the indices in new_t by boost_by, and take the newly created
 * node to have id = 1 + top_id of boosted new_t.
 *)
let add_boosted_subtree_above ~t ~new_t where boost_by = 
  let our_top_id = top_id t.tree in
  let new_top_bl = (get_bl t our_top_id) -. where in
  let boosted_new_t = boost boost_by new_t in
  let new_id = 1 + top_id (boosted_new_t.tree) in
  (* if new_top_bl is neg then highest_distal was bigger than top edge *)
  assert(new_top_bl >= 0.);
  bl_join2
    {t with info = opt_add_info our_top_id ~bl:where t.info}
    boosted_new_t
    new_id
    new_top_bl

(* add a collection of subtrees given a list of (where, tree) pairs; where
 * describes where the tree should be glued in. pos is the "where" of the
 * previous lowest tree.
 * we assume that the top_id for each of the subtrees is equal to the number of
 * internal nodes of new_t minus one, as it would be from a postorder traversal,
 * starting from zero.
 * we assume that all ids >= avail_id are available.
 *)
let add_subtrees_above avail_id tree where_subtree_list = 
  let rec aux pos accu accu_id = function
    | [] -> accu_id, accu
    | (new_pos, new_t)::rest ->
        aux
          new_pos
          (add_boosted_subtree_above 
            ~t:accu 
            ~new_t 
            (new_pos -. pos)
            accu_id)
          (* 2 = 1 for internal node + 1 to get to next avail *)
          (2 + accu_id + (top_id new_t.tree))
          rest
  in
  aux 
    0. 
    tree
    avail_id
    (List.sort
      (fun pos_t1 pos_t2 -> compare (fst pos_t1) (fst pos_t2))
      where_subtree_list)

(* we assume that all input trees have their maximal ids at the top *)
let add_subtrees_by_map ref_tree where_subtree_map = 
  let global_avail_id = ref (1+(top_id ref_tree.tree)) in
  let globalized_id_add_subtrees_above tree where_subtree_list = 
    let new_id, result = 
      add_subtrees_above !global_avail_id tree where_subtree_list in
    global_avail_id := new_id;
    result
  in
  (* add the info from ref tree back in and then add above subtrees *)
  let our_add_above below = 
    let id = top_id below.tree in
    globalized_id_add_subtrees_above 
      (istree_copy_info ~src:ref_tree ~dest:below id)
      (if IntMap.mem id where_subtree_map then 
        IntMap.find id where_subtree_map
      else [])
  in
  (* fill the stree skeleton back in with info and add subtrees *)
  let rec aux = function
    | Node(i, tL) -> our_add_above (info_join i (List.map aux tL))
    | Leaf i -> our_add_above (inform_stree (Leaf i) emptyInfo)
  in
  aux (get_tree ref_tree)



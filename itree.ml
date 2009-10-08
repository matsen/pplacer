(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

(* stree.ml
 * We number all of the nodes and then have maps which tell us about the edges.
 * Note that the edge information is for the node directly above the node with
 * the chosen edge.
 * *)


open MapsSets

type itree = 
  {stree : Stree.stree;
  info : Itree_info.itree_info}

let itree t info = {stree = t; info = info}

let get_an_info get_what tree = 
  get_what tree.info

let get_stree ist = ist.stree
let get_info ist = ist.info
let get_taxon = get_an_info Itree_info.get_taxon
let get_boot = get_an_info Itree_info.get_boot
let get_bl = get_an_info Itree_info.get_bl

let top_id t = Stree.top_id (get_stree t)

(* copy the info from src at id over to dest *)
let istree_copy_info ~dest ~src id = 
  itree 
    (get_stree dest)
    (Itree_info.copy_info 
      ~dest:(get_info dest) 
      ~src:(get_info src) 
      id)

let tree_length tree = 
  let get_our_bl id = get_bl tree id in
  let rec aux = function
    | Stree.Node(id, tL) ->
        List.fold_left 
          ( +. ) 
          (get_our_bl id)
          (List.map aux tL)
    | Stree.Leaf id -> get_our_bl id
  in
  match get_stree tree with 
  | Stree.Node(_, tL) ->
      (* exclude the root edge *)
      List.fold_left ( +. ) 0. (List.map aux tL)
  | Stree.Leaf _ -> 0.

(* make the bootstrap value into the node number *)
let make_boot_node_num tree = 
  {tree with info = 
    {tree.info with Itree_info.boot = 
      List.fold_right
        (fun n -> IntMap.add n (float_of_int n))
        (Stree.collect_node_numbers tree.stree)
        IntMap.empty}}

(* join a list of info_trees *)
let join new_id tL = 
  itree
    (Stree.node new_id (List.map get_stree tL))
    (Base.complete_fold_left
      Itree_info.combine_node_infos 
      (List.map get_info tL))

let boost by ist = 
  itree
    (Stree.boost by (get_stree ist))
    (Itree_info.boost by (get_info ist))

(* join two trees and add a branch length *)
let bl_join2 t1 t2 new_id bl = 
  let without_bl = join new_id [t1; t2] in
  { without_bl with
  info = Itree_info.opt_add_info ~bl new_id without_bl.info}

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
  let our_top_id = top_id t in
  let new_top_bl = (get_bl t our_top_id) -. where in
  let boosted_new_t = boost boost_by new_t in
  let new_id = 1 + top_id boosted_new_t in
  (* if new_top_bl is neg then highest_distal was bigger than top edge *)
  assert(new_top_bl >= 0.);
  bl_join2
    {t with info = Itree_info.opt_add_info our_top_id ~bl:where t.info}
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
          (2 + accu_id + (top_id new_t))
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
  let global_avail_id = ref (1+(top_id ref_tree)) in
  let globalized_id_add_subtrees_above tree where_subtree_list = 
    let new_id, result = 
      add_subtrees_above !global_avail_id tree where_subtree_list in
    global_avail_id := new_id;
    result
  in
  (* add the info from ref tree back in and then add above subtrees *)
  let our_add_above below = 
    let id = top_id below in
    globalized_id_add_subtrees_above 
      (istree_copy_info ~src:ref_tree ~dest:below id)
      (if IntMap.mem id where_subtree_map then 
        IntMap.find id where_subtree_map
      else [])
  in
  (* fill the stree skeleton back in with info and add subtrees *)
  let rec aux = function
    | Stree.Node(i, tL) -> our_add_above (join i (List.map aux tL))
    | Stree.Leaf i -> 
        our_add_above (itree (Stree.Leaf i) Itree_info.empty_info)
  in
  aux (get_stree ref_tree)



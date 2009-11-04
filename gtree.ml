(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * our tree data stucture with information.
 *
 * Gtree stands for general tree, meaning that we have a tree with any kind of
 * bark that has get/set methods for bl, name, and boot.
 *
 * We number all of the nodes and then have maps which tell us about the edges.
 * Note that the edge information is for the node directly above the node with
 * the chosen edge.
 * *)

open Fam_batteries
open MapsSets

type 'a gtree = 
  {stree : Stree.stree;
  bark_map : 'a IntMap.t}

let gtree stree bark_map = 
  {stree = stree; bark_map = bark_map}

let of_stree stree = {stree = stree; bark_map = IntMap.empty}

let get_stree t = t.stree
let get_bark_map t = t.bark_map
let get_bark t id = IntMap.find id t.bark_map
let get_bark_opt t id = 
  if IntMap.mem id t.bark_map then Some(get_bark t id) else None
let get_bl t id = (get_bark t id)#get_bl
let get_name t id = (get_bark t id)#get_name
let get_boot t id = (get_bark t id)#get_boot

let set_bark_map t bark_map = {t with bark_map = bark_map}
let add_bark t id bark = 
  set_bark_map t (IntMap.add id bark (get_bark_map t))

let set_bl t id bl = 
  add_bark t id ((get_bark t id)#set_bl bl)

(* stree related *)
let top_id t = Stree.top_id (get_stree t)
let n_edges t = Stree.n_edges (get_stree t)
let recur f_node f_leaf t = Stree.recur f_node f_leaf (get_stree t)

(* bark map related *)
let add_bark id b t = 
  { t with bark_map = (IntMap.add id b (get_bark_map t)) }
let map_bark_map f t = 
  {t with bark_map = IntMap.map f (get_bark_map t)}

(* general *)

let compare bark_compare t1 t2 = 
  try 
    Base.raise_if_different compare (get_stree t1) (get_stree t2);
    Base.raise_if_different 
      (IntMap.compare bark_compare)
      (get_bark_map t1)
      (get_bark_map t2);
    0
  with
  | Base.Different c -> c

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

(* copy the info from src at id over to dest *)
let copy_bark ~dest ~src id = 
  gtree 
    (get_stree dest)
    (IntMap.add 
      id 
      (IntMap.find id (get_bark_map src)) 
      (get_bark_map dest))


(* join a list of info_trees *)
let join new_id tL = 
  gtree
    (Stree.node new_id (List.map get_stree tL))
    (ListFuns.complete_fold_left
      Bark_map.union 
      (List.map get_bark_map tL))

let boost by t = 
  gtree
    (Stree.boost by (get_stree t))
    (Bark_map.boost by (get_bark_map t))

(* join two trees and add bark to the top *)
let bark_join2 t1 t2 new_id bark = 
  let without_bl = join new_id [t1; t2] in
  add_bark new_id bark without_bl 

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

let add_boosted_subtree_above bark_of_bl ~t ~new_t where boost_by = 
  let our_top_id = top_id t in
  let new_top_bl = (get_bl t our_top_id) -. where in
  let boosted_new_t = boost boost_by new_t in
  let new_id = 1 + top_id boosted_new_t in
  (* if new_top_bl is neg then highest_distal was bigger than top edge *)
  assert(new_top_bl >= 0.);
  bark_join2
    (set_bl t our_top_id where)
    boosted_new_t
    new_id
    (bark_of_bl new_top_bl)

(* add a collection of subtrees given a list of (where, tree) pairs; where
 * describes where the tree should be glued in. pos is the "where" of the
 * previous lowest tree.
 * we assume that the top_id for each of the subtrees is equal to the number of
 * internal nodes of new_t minus one, as it would be from a postorder traversal,
 * starting from zero.
 * we assume that all ids >= avail_id are available.
 *)
let add_subtrees_above bark_of_bl avail_id tree where_subtree_list = 
  let rec aux pos accu accu_id = function
    | [] -> accu_id, accu
    | (new_pos, new_t)::rest ->
        aux
          new_pos
          (add_boosted_subtree_above 
            bark_of_bl
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
      (fun pos_t1 pos_t2 -> 
        Pervasives.compare (fst pos_t1) (fst pos_t2))
      where_subtree_list)

(* we assume that all input trees have their maximal ids at the top *)
let add_subtrees_by_map bark_of_bl ref_tree where_subtree_map = 
  (* here we keep track of the available ids so that we can use new ones *)
  let global_avail_id = ref (1+(top_id ref_tree)) in
  let globalized_id_add_subtrees_above tree where_subtree_list = 
    let new_id, result = 
      add_subtrees_above bark_of_bl !global_avail_id tree where_subtree_list in
    global_avail_id := new_id;
    result
  in
  (* add the info from ref tree back in and then add above subtrees *)
  let our_add_above below = 
    let id = top_id below in
    globalized_id_add_subtrees_above 
      (copy_bark ~src:ref_tree ~dest:below id)
      (if IntMap.mem id where_subtree_map then 
        IntMap.find id where_subtree_map
      else [])
  in
  (* fill the stree skeleton back in with info and add subtrees *)
  let rec aux = function
    | Stree.Node(i, tL) -> our_add_above (join i (List.map aux tL))
    | Stree.Leaf i -> our_add_above (of_stree (Stree.Leaf i))
  in
  aux (get_stree ref_tree)


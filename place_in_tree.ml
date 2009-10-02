(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets
open Fam_batteries
open Stree
open Placement

(* single placement *)
let place_single new_name absol_place istree = 
  let id = absol_place.location in
  let pendant_id = 1 + (max_id istree.tree) in
  let internal_id = 1 + pendant_id in
  let rec aux tree = 
    if id = top_id tree then
      Node(internal_id, [Leaf pendant_id; tree])
    else match tree with
    | Node(i, tL) -> Node(i, List.map aux tL)
    | Leaf(i) -> Leaf(i)
  in
  flush_all ();
  let new_stree = aux istree.tree in
  if new_stree = istree.tree then 
    failwith (Printf.sprintf "place_single_in_tree: id %d not found!" id);
  let new_bl_above_internal = 
    (IntMap.find id istree.info.bl) -. absol_place.distal_bl in
  {tree = new_stree;
  info = {istree.info with 
           taxon = IntMap.add pendant_id new_name istree.info.taxon;
           bl = IntMap.add internal_id new_bl_above_internal (
                      IntMap.add id absol_place.distal_bl (
                      IntMap.add pendant_id absol_place.pendant_bl 
                        istree.info.bl))}}
      
(* gen_lots_place:
 * general placement.
 * tree_and_info_of_places takes a collection of placements which are at a given
 * node and returns a representation of them.
 * *)
let gen_lots_place tree_and_info_of_places npcl_map ref_istree = 
  (* first we have a fun which recurs below *)
  let rec aux start_avail_id (start_tree : stree) (start_info : node_info) = 
    let (new_avail_id, new_this_tree, new_this_info) = 
      match start_tree with
      (* if we encounter a tree, recur through it *)
      | Node(id, tL) -> 
          let (below_avail_id, below_info, below_trees) = List.fold_right (
            fun (tree:stree) (prev_a_id, prev_info, done_trees) ->
              let (next_a_id, next_done_tree, next_info) = 
                aux prev_a_id tree prev_info in
              (next_a_id, next_info, next_done_tree::done_trees)) 
            tL (start_avail_id, start_info, []) in
          (below_avail_id, Node(id, below_trees), below_info)
      (* Leaf: nothing to recur *)
    | Leaf id -> (start_avail_id, Leaf id, start_info)
    in
    let id = top_id start_tree in
    if IntMap.mem id npcl_map then begin
      (* we have something to add at this node *)
      let our_bl = Stree.info_get_bl new_this_info id 
      and placed_here = IntMap.find id npcl_map in
      let final_avail_id, place_tree, place_info = 
        tree_and_info_of_places new_avail_id new_this_info id placed_here in
     (final_avail_id+1,
     Node(final_avail_id, [new_this_tree; place_tree]),
     {place_info 
     (* half the branch length to the old node *)
      with bl = IntMap.add id (our_bl /. 2.) (
        (* and half to the new node *)
        IntMap.add final_avail_id (our_bl /. 2.) place_info.bl)})
    end
    else (new_avail_id, new_this_tree, new_this_info)
  in
  let (_, total_tree, total_info) = 
    aux (1 + max_id ref_istree.tree) ref_istree.tree ref_istree.info in
  {tree = total_tree; info = total_info}

let get_top_placement = function
  | place::_ -> place
  | [] -> failwith "empty placement list!"

let check_placements placement_loc places = 
  (* make sure they are in the right place *)
  List.iter 
    (fun (_, places) -> 
      assert((get_top_placement places).location = placement_loc))
    places
    
(* number placement *)
let tree_and_info_of_number_places 
      bogus_bl avail_id start_info placement_loc places =
  check_placements placement_loc places;
  (1+avail_id, (* just add one pendant node *)
  Leaf avail_id, (* that pendant node *)
  {start_info with (* and some additional info *)
    taxon = IntMap.add avail_id (
      Printf.sprintf "%d_at_%d" (List.length places) placement_loc)
      start_info.taxon;
    bl = IntMap.add avail_id bogus_bl start_info.bl;})
 
let number_place bogus_bl npcl_map ref_istree = 
  gen_lots_place (tree_and_info_of_number_places bogus_bl) npcl_map ref_istree

(* together placement *)
let tree_and_info_of_together_places 
    bogus_bl avail_id start_info placement_loc npcl = 
  check_placements placement_loc npcl;
  let (final_id, final_info, trees) =
    List.fold_right (
      fun (name, places) (sofar_id, sofar_info, sofar_trees) ->
        let place = get_top_placement places in
        (sofar_id+1,
        {sofar_info with 
        taxon = IntMap.add sofar_id name sofar_info.taxon;
        bl = IntMap.add sofar_id place.pendant_bl sofar_info.bl},
        (Leaf sofar_id)::sofar_trees)
    ) npcl (avail_id, start_info, []) in
  match trees with 
  | [] -> assert(false)
  | [pend] ->
    (* just a pendant edge. no internal node *)
    (final_id, pend, final_info)
  | _::_ -> 
    (* we have added an actual tree, and need to increase the id by one to
     * account for the new internal node *)
    (final_id+1,
    Node(final_id, trees), 
    {final_info with bl = IntMap.add final_id bogus_bl final_info.bl})

let together_place bogus_bl npcl_map ref_istree = 
  gen_lots_place (tree_and_info_of_together_places bogus_bl) npcl_map ref_istree

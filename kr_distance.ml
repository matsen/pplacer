(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Here we have the core agorithm, which takes two placement collection lists
 * and calculates their weighted KR distance.
 *
 * A placement collection list (pcl) is a list (queries) of placement lists
 * (list of placements for a given query). 
 *
 * A "KR info" is a vector which gives the weight of an individual placement.
 * Each query is allocated 1/(num queries), and this get broken up by the
 * different placements according to their weight. Note that if a placement
 * collection has only one entry (list of length one) then all of the weight
 * will go onto that placement. This will be true when we are not doing the
 * weighted version.
 *
 * These KR info vectors get totalled as we proceed up the tree, and their
 * exponentiated difference gets multiplied by the length of the segment without
 * any placements. This gives the total KR distance.
*)

open Fam_batteries
open MapsSets
open Mokaphy_prefs

exception Invalid_place_loc of float
exception Total_kr_not_zero of float

let tol = 1e-10 (* "zero" *)

(* the KR exponent is 1/p unless p is less than 1, in which case it's 1 *)
let outer_exponent p = 
  if p < 1. then 1.
  else 1. /. p

let collect_kr_info criterion kr_v pcl =
  List.flatten
    (List.map
      (function pc ->
        List.map2
          (fun place weight ->
            (Placement.location place,
            (Placement.distal_bl place, 
            Mokaphy_base.v_times_scalar kr_v weight)))
          pc
          (Base.normalized_prob (List.map criterion pc)))
       pcl)
 
(* exp_kr_diff is the difference of the two prob dists to the pth pow *)
let exp_kr_diff p kr_v = (abs_float (kr_v.(0) -. kr_v.(1))) ** p

(* total up the info from the previous step. 
 * note that data_sofar will be modified in place. 
 * data_to_r: takes the kr info vector and turns it into a real number
 * data_info_list: list of (distal_bl, data)
 *
 * signature is
('a -> float) -> float -> (float * 'b) list -> ('a -> 'b -> 'c) -> float ->     'a          -> float * 'a
 data_to_r       bl       data_info_list        update_data        prev_subtot  start_data  
 *
 * is what ocaml inferrs, but update_data is actually 'a -> 'b -> unit, as
 * update_data modifies in place
 * *)
let total_along_edge data_to_r bl data_info_list update_data prev_subtot start_data = 
  let rec aux ~subtotal ~prev_a data_sofar data_info_list = 
    (* next_total actually adds on the segment length times data_to_r of
     * the kr vector *)
    let next_subtotal a =
      let seg_len = a -. prev_a in
      (* Printf.printf "%g\t%g\n" prev_a a; *)
      assert(seg_len >= 0.);
      subtotal+.seg_len*.(data_to_r data_sofar)
    in
    match data_info_list with
    (* a is the location of the location of the data along the edge *)
    | (a, data)::rest -> 
        (* we pull this out so that we do the next total, then add on the data
         * onto the data_sofar *)
        if a < 0. || a > bl then raise (Invalid_place_loc a);
        let the_next_subtotal = next_subtotal a in
        aux
          ~subtotal:the_next_subtotal
          ~prev_a:a
          (update_data data_sofar data; data_sofar)
          rest
    | [] -> 
        (* sum things up on final segment to the next node *)
        (next_subtotal bl, data_sofar)
  in
  aux prev_subtot 0. start_data data_info_list

(* total some data over the tree, which can be combined from subtrees using
 * data_list_sum, and can be totaled across edges using curried_edge_total, and
 * starts at leaves with starter_data_factory.
 * the reason why we use starter_data_factory rather than doing a fully
 * functional approach is that then the number of allocations is linear in only
 * the size of the tree, rather than depending on the number of placements .
 * *)
let total_over_tree curried_edge_total
                    check_final_data
                    data_list_sum
                    starter_data_factory 
                    ref_tree =
  let (grand_total, final_data) = 
    Stree.recur 
      (fun id below_list -> (* the node recurrence *)
        curried_edge_total 
          id
          (List.fold_right ( +. ) (List.map fst below_list) 0.) (* prev subtot *)
          (data_list_sum (List.map snd below_list))) (* total of below kr_infos *)
      (fun id ->
        curried_edge_total 
          id
          0. 
          (starter_data_factory ()))
      (Itree.get_stree ref_tree)
  in
  check_final_data final_data;
  grand_total /. (Itree.tree_length ref_tree)

(* sort the placements along a given edge according to their location on
 * the edge in an increasing manner. that way we can recur along this list,
 * moving towards the root. *)
let sort_along_edge = 
  IntMap.map
    (List.sort (fun (a1,_) (a2,_) -> compare a1 a2))

(* get the KR distance between two placement collection lists *)
let pcl_pair_distance criterion ref_tree p pcl1 pcl2 = 
  let int_inv x = 1. /. (float_of_int x) in
  (* these two may take arguments in the future *)
  let kr_v1 = [|int_inv (List.length pcl1); 0.|]
  and kr_v2 = [|0.; int_inv (List.length pcl2)|]
  in
  (* this map has all of the information needed to do the KR calculation *)
  let all_kr_map = 
    sort_along_edge
      (IntMapFuns.of_pairlist_listly
        ((collect_kr_info criterion kr_v1 pcl1) @
         (collect_kr_info criterion kr_v2 pcl2)))
  in
  (* ppr_kr_info Format.std_formatter all_kr_map; Format.pp_print_newline Format.std_formatter ()
  *)
  (* total across all of the edges of the tree *)
  let starter_kr_v = [|0.; 0.|]
   in
  let kr_edge_total id = 
    total_along_edge 
      (exp_kr_diff p) 
      (Itree.get_bl ref_tree id) 
      (Mokaphy_base.get_from_list_intmap all_kr_map id)
      Mokaphy_base.v_addto
  (* make sure that the kr_v totals to zero *)
  and check_final_kr final_kr_v = 
    let final_kr_diff = final_kr_v.(0) -. final_kr_v.(1) in
    if abs_float final_kr_diff > tol then 
      raise (Total_kr_not_zero final_kr_diff)
  in
  (total_over_tree 
    kr_edge_total
    check_final_kr
    Mokaphy_base.v_list_sum
    (fun () -> Array.copy starter_kr_v)
    ref_tree)
  ** (outer_exponent p)

(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * do we want to do the unrooted tree? 
 * we could search moving around the stree.
*)

open MapsSets
open Fam_batteries

type direction = Distal | Proximal

(* masses *)
let list_sum = List.fold_left (fun accu x -> accu +. x) 0.

let submap m key_list = 
  List.fold_right
    (fun k accu -> 
      if IntMap.mem k m then
        IntMap.add k (IntMap.find k m) accu
      else
        accu)
    key_list
    IntMap.empty

let singleton_map k v = IntMap.add k v IntMap.empty

let total_mass mass_map = 
  IntMap.fold 
    (fun _ mass_l accu -> 
      List.fold_right 
        (fun (_, m) -> ( +. ) m)
        mass_l
        accu)
    mass_map
    0.

let list_count f l = 
  List.fold_left 
    (fun accu x -> if f x then accu+1 else accu)
    0
    l


(*
# let t = Gtree.get_stree (Newick.of_string "((a,b),(c,d))");;
val t : Stree.stree = ((0,1)2,(3,4)5)6
# let ids = Barycenter.collect_distal_ids t 2;;
val ids : int list = [0; 1; 2]
# let ids = Barycenter.collect_proximal_ids t 2;;
val ids : int list = [6; 5; 3; 4]
*)
let collect_distal_ids stree wanted = 
  let rec aux = function
    | Stree.Node(i, tL) as sub ->
        if i = wanted then Stree.collect_node_numbers sub
        else (
          let below = List.map aux tL in
(* make sure we don't have the id appearing multiple places *)
          assert(list_count (( <> ) []) below <= 1);
          List.flatten below)
    | Stree.Leaf i -> if i = wanted then [i] else []
  in 
  aux stree

let collect_proximal_ids stree wanted = 
  let rec aux = function
    | Stree.Node(i, tL) ->
        if i = wanted then []
        else (i :: (List.flatten (List.map aux tL)))
    | Stree.Leaf i -> if i = wanted then [] else [i]
  in 
  aux stree

let tree_work collect_fun p ref_tree mass edge_id = 
  let sub_mass = 
    submap 
      mass 
      (collect_fun (Gtree.get_stree ref_tree) edge_id) 
  in
  Kr_distance.dist
    ref_tree
    p
    sub_mass
    (singleton_map
      edge_id
      [Gtree.get_bl ref_tree edge_id, total_mass sub_mass])

let distal_work p ref_tree = 
  tree_work collect_distal_ids p ref_tree

let proximal_work p ref_tree = 
  tree_work collect_proximal_ids p ref_tree

let work_diff p ref_tree mass edge_id = 
  let run f = f p ref_tree mass edge_id in
  (run distal_work) -. (run proximal_work)

let ppr_opt_int_list =
  Ppr.ppr_list (Ppr.ppr_opt Format.pp_print_int) 

let find p ref_tree mass = 
  (* return the id iff the work diff on that edge is positive *)
  let diff id = 
    let d = work_diff p ref_tree mass id in
    Printf.printf "at %d we get %g\n" id d;
    if d < 0. then None else Some id
  in
  let rec aux = function
    | Stree.Leaf id -> diff id
    | Stree.Node(id, tL) ->
      (match diff id with
      | None -> None
      | Some _ ->
    (* pos work diff, so we try moving lower *)
        (let below = (List.map aux tL) in
        Format.fprintf Format.std_formatter "%a\n" ppr_opt_int_list below;
        match List.filter (( <> ) None) below with
        | [] -> 
            (* nothing good below, so stop here *)
            Some id
        | [ x ] -> 
            (* found something good below *)
            x
        | l -> 
            if List.length tL = List.length l then 
              (* the node (not the edge) is the barycenter *)
              Some id
            else
              (ppr_opt_int_list Format.std_formatter l;
              failwith "convexity problem!")))
  in
  aux (Gtree.get_stree ref_tree)



    (*
let work_in_tree p t mass = 


let prx = Placerun_io.of_file 

*)

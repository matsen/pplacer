(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * The unfortunate thing here is that we really do have to write two codes: one
 * for finding the right edge on the tree, and another for finding the right
 * position on that edge. We *could* add edges in then remove them later, but
 * that has its own perils and have settled with a bit of code dup.
 *
*)

open MapsSets
open Fam_batteries

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

(* the total mass in the mass_map *)
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
    (fun accu x -> if f x then accu+1 else accu) 0 l

(*
# let t = Gtree.get_stree (Newick.of_string "((a,b),(c,d))");;
val t : Stree.stree = ((0,1)2,(3,4)5)6
# let ids = Barycenter.collect_distal_ids t 2;;
val ids : int list = [0; 1; 2]
# let ids = Barycenter.collect_proximal_ids t 2;;
val ids : int list = [6; 5; 3; 4]
*
* note that distal includes "wanted".
*)
let collect_distal_ids stree wanted = 
  let rec aux = function
    | Stree.Node(i, tL) as sub ->
        if i = wanted then Stree.collect_node_numbers sub
        else 
          (let below = List.map aux tL in
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

(* the amount of work required to move the mass in "mass" map to the distal side
 * of the edge labeled edge_id *)
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

(* distal includes the chosen edge *)
let distal_work p ref_tree = 
  tree_work collect_distal_ids p ref_tree

let proximal_work p ref_tree = 
  tree_work collect_proximal_ids p ref_tree

(* positive when distal beats proximal. that means that we should stop when this
 * value becomes positive. *)
let work_diff p ref_tree mass edge_id = 
  let run f = f p ref_tree mass edge_id in
  (run distal_work) -. (run proximal_work)

let ppr_opt_int_list =
  Ppr.ppr_list (Ppr.ppr_opt Format.pp_print_int) 

(* find the edge containing the barycenter *)
let find_edge p ref_tree mass = 
  (* return the id iff the work diff on that edge is positive. Some id means
   * that we go investigate below the id edge. *)
  let diff id = 
    let d = work_diff p ref_tree mass id in
    (* Printf.printf "at %d we get %g\n" id d; *)
    if d < 0. then None else Some id
  in
  let rec aux = function
    | Stree.Leaf id -> diff id
    | Stree.Node(id, tL) ->
      (match diff id with
      | None -> None
      | Some _ ->
      (* pos work diff, so we try moving lower *)
        (match List.filter (( <> ) None) (List.map aux tL) with
        | [] -> 
            (* nothing good below, so stop here *)
            Some id
        | [ x ] -> 
            (* found something good below. return it. *)
            x
        | l -> 
            if List.length tL = List.length l then 
              (* the node (not the edge) is the barycenter *)
              Some id
            else
              (ppr_opt_int_list Format.std_formatter l;
              failwith "convexity problem!")))
  in
  match aux (Gtree.get_stree ref_tree) with
  | Some edge_id -> edge_id
  | None -> failwith "find_edge failed!"

(* find the edge and the location along the edge.
 * we move along the edge from the bottom to the top, and stop when appropriate.
 * this could be made faster by special handling of the masses, i.e. not using
 * the kr_distance function. but that would take lots more code.
 * *)
let find p ref_tree unsorted_mass = 
  let mass = Mass_map.Indiv.sort unsorted_mass in
  let edge_id = find_edge p ref_tree mass in
  let get_sub_mass collect_fun = 
    submap mass (collect_fun (Gtree.get_stree ref_tree) edge_id) 
  in
(* the first step is to get the distal and proximal maps for the mass not
 * on the given edge. in contrast to before these are the true distal and
 * proximal, so we don't include the chosen edge in the mass map *)
  let distal_mass = 
    IntMap.remove edge_id (get_sub_mass collect_distal_ids) 
  and proximal_mass = get_sub_mass collect_proximal_ids 
  (* we also get the mass on our edge. this is what we will be cycling through. *)
  and our_mass_list = 
    if IntMap.mem edge_id mass then IntMap.find edge_id mass
    else []
  and bl = Gtree.get_bl ref_tree edge_id in
  (* here and below, pos refers to the position along the edge, measured from
   * the distal side *)
  let work sub_mass pos = 
    Kr_distance.dist
      ref_tree
      p
      sub_mass
      (singleton_map
        edge_id
        [pos, total_mass sub_mass])
  in
  (* our loop. we are at a place on the edge which has our_distal_mass_list
   * below us, and our_proximal_mass_list above us. we add those to our
   * respective maps so we can calculate with kr_distance. *)
  let rec aux curr_pos our_distal_mass_list our_proximal_mass_list = 
    (* Printf.printf "curr_pos is %g\n" curr_pos; *)
    let add_mass extra_mass mass_map =
      IntMap.add edge_id extra_mass mass_map in
    (* add mass to the maps as mentioned above *)
    let dm = add_mass our_distal_mass_list distal_mass
    and pm = add_mass our_proximal_mass_list proximal_mass
    in
    (* the amount by which the work moving the proximal mass to pos exceeds that
     * moving the distal mass to pos. the sign is reversed when compared to
     * the "edge" version but that way we again stop when it becomes negative. *)
    (* da and dc are Delta(a) and Delta(c) in the barycenter scan *)
    let delta pos = (work pm pos) -. (work dm pos) in
    let dc = delta curr_pos in
    (* Printf.printf "dc = %g\n" dc; *)
    assert(curr_pos <= bl);
    assert(dc >= 0.);
    (* the barycenter formula. because da is negative (as we don't want to move
     * farther up) we are taking a weighted average here. 
     * wp + mp * x = wd + md * (bl - x)
     * (mp - md) * x = md * bl + wd + wp
     * *)
    let bary ~above_pos da = 
      assert(da <= 0.);
      curr_pos +. (above_pos -. curr_pos) *. dc /. (dc -. da)
    in
    match our_proximal_mass_list with
    | [] -> (* at the top of the edge *)
        (edge_id, bary ~above_pos:bl (delta bl))
    | (pos, mass)::rest -> begin
        let da = delta pos in
        if da > 0. then
          (* we can do better by moving past this placement *)
          aux pos ((pos,mass)::our_distal_mass_list) rest
        else
          (edge_id, bary ~above_pos:pos da)
    end
  in
  aux 0. [] our_mass_list

let of_placerun weighting criterion p pr = 
  find 
    p 
    (Placerun.get_ref_tree pr)
    (Mass_map.Indiv.of_placerun 
      weighting
      criterion
      pr)

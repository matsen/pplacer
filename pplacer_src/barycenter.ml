(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
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

let extract_goods l =
  List.fold_left
    (fun accu -> function | Some x -> x::accu | None -> accu) [] l


(*
# let t = Gtree.get_stree (Newick.of_string "((a,b),(c,d))");;
val t : Stree.stree = ((0,1)2,(3,4)5)6
# let ids = Barycenter.collect_distal_ids t 2;;
val ids : int list = [0; 1]
# let ids = Barycenter.collect_proximal_ids t 2;;
val ids : int list = [6; 5; 3; 4]
*
* note that neither of them include "wanted."
*)
let collect_distal_ids stree wanted =
  let rec aux = function
    | Stree.Node(i, tL) ->
        if i = wanted then
          List.flatten (List.map Stree.node_ids tL)
        else
          (let below = List.map aux tL in
(* make sure we don't have the id appearing multiple places *)
          assert(list_count (( <> ) []) below <= 1);
          List.flatten below)
(* if we get to a leaf but haven't hit wanted yet then the leaf must be wanted.
 * however, in that case we don't include it by the above comment *)
    | Stree.Leaf _ -> []
  in
  aux stree

let collect_proximal_ids stree wanted =
  let rec aux = function
    | Stree.Node(i, tL) ->
        if i = wanted then [] (* stop *)
        else (i :: (List.flatten (List.map aux tL)))
    | Stree.Leaf i -> if i = wanted then [] else [i]
  in
  aux stree

exception Found_edge of int
exception Found_node of int
type action = Continue | Stop

let find ref_tree mass_m =
  let smass = Mass_map.Indiv.sort mass_m in
  let work sub_mass id pos =
    Kr_distance.dist ref_tree 1.
      sub_mass
      (singleton_map id [pos, total_mass sub_mass])
  in
  let get_sub_mass collect_fun id =
    submap smass (collect_fun (Gtree.get_stree ref_tree) id)
  in
  (* the amount of work required to move all of the mass on the chosen side, as
   * well as the edge_mass, to pos on id *)
  let tree_work collect_fun edge_mass id pos =
    let sub_mass =
      IntMap.add id edge_mass (get_sub_mass collect_fun id)
    in
    work sub_mass id pos
  in
  let proximal_work prox_ml id pos =
    List.iter (fun (m_pos,_) -> assert(m_pos >= pos)) prox_ml;
    tree_work collect_proximal_ids prox_ml id pos
  and distal_work dist_ml id pos =
    List.iter (fun (m_pos,_) -> assert(m_pos <= pos)) dist_ml;
    tree_work collect_distal_ids dist_ml id pos
  in
  (* prox_ml is extra mass that is thought of as living on the proximal side of
   * the edge. as used below, it is the mass that is proximal to pos on the
   * edge. equivalent for dist_ml. *)
  let delta ~prox_ml ~dist_ml id pos =
    (proximal_work prox_ml id pos) -. (distal_work dist_ml id pos)
  in
  let get_mass_list id =
    if IntMap.mem id smass then IntMap.find id smass
    else []
  in
  (* this is the function that helps us find the barycenter-containing edge *)
  let check id =
    let bl = Gtree.get_bl ref_tree id
    and our_mass_list = get_mass_list id in
    if 0. > delta ~prox_ml:our_mass_list ~dist_ml:[] id 0.
    (* we are negative at the bottom of the edge. continue. *)
    then Continue
    else if 0. > delta ~prox_ml:[] ~dist_ml:our_mass_list id bl
    (* top is negative (and bottom is positive from before) *)
    then raise (Found_edge id)
    else
    (* top is positive *)
      Stop
  in
  (* find the location, i.e. edge or node where the barycenter lies *)
  let rec find_loc = function
    | Stree.Leaf id -> check id
    | Stree.Node(id, tL) ->
        (match check id with
        | Continue ->
            let below = List.map find_loc tL in
  (* this edge is negative at the bottom but nothing was found.
   * the deepest node to have this must have positive at the tops of all of the
   * edges. we assert to make sure this is the case. *)
            List.iter (fun b -> assert(b = Stop)) below;
            raise (Found_node id)
        | Stop as s -> s)
  in
  (* da and dc are Delta(a) and Delta(c) in the barycenter scan *)
  let find_pos id =
    let bl = Gtree.get_bl ref_tree id in
    let rec aux ~dist_ml ~prox_ml curr_pos =
      let our_delta pos =
        assert(pos <= bl);
        delta ~dist_ml ~prox_ml id pos
      in
    (* the barycenter formula. because da is negative we are essentially taking
     * a weighted average here. *)
      let bary ~above_pos da =
        assert(da <= 0.);
        let dc = our_delta curr_pos in
        assert(dc >= 0.);
        curr_pos +. (above_pos -. curr_pos) *. dc /. (dc -. da)
      in
      match prox_ml with
      | [] -> (* at the top of the edge *)
          bary ~above_pos:bl (our_delta bl)
      | (pos, mass)::rest -> begin
        (* pos is now the next position up *)
        let da = our_delta pos in
        if da > 0. then
          (* we can do better by moving past this placement *)
          aux
            ~dist_ml:((pos,mass)::dist_ml)
            ~prox_ml:rest
            pos
        else
          bary ~above_pos:pos da
      end
    in
    (* start at the bottom with all the placements on top *)
    aux ~dist_ml:[] ~prox_ml:(get_mass_list id) 0.
  in
  try
    let _ = find_loc (Gtree.get_stree ref_tree) in
    failwith "failed to find barycenter edge/node!"
  with
  | Found_node id ->
      (* the node is at the bottom of the edge *)
      (id, 0.)
  | Found_edge id ->
      (* let pos_classic = find_pos id  *)
      (*
      let pos_new =
        delta ~dist_ml:(get_mass_list id) ~prox_ml:[] id (Gtree.get_bl ref_tree id)
      in
      Printf.printf "%g\t%g\n" pos_classic pos_new; *)
      (id, find_pos id)

(* pre means pre mass map *)
let of_pre transform t pmm = find t (Mass_map.Indiv.of_pre transform pmm)

let of_placerun transform weighting criterion pr =
  find
    (Placerun.get_ref_tree pr)
    (Mass_map.Indiv.of_placerun
      transform
      weighting
      criterion
      pr)


(* ** for measuring the difference between two placements ** *)

(* to get the distance between two locations on a tree. a bit of a silly hack to
 * make a distance matrix then just get a single distance, but it works *)
let location_distance t (loc1, distal1) (loc2, distal2) =
  let make_place loc dist_bl =
    Placement.make_ml loc ~ml_ratio:1. ~log_like:1.
                          ~dist_bl ~pend_bl:0.
  in
  let m =
    Distance_mat.of_placement_array t
      [| make_place loc1 distal1;
         make_place loc2 distal2; |]
  in
  Uptri.get m 0 1


let calc_dist transform weighting criterion pr1 pr2 =
  let calc_bary pr =
    of_placerun transform weighting criterion pr
  in
  location_distance
    (Placerun.get_same_tree pr1 pr2)
    (calc_bary pr1)
    (calc_bary pr2)


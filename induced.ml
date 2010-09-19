(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPEED: replace sort in the induced maker by something which just takes the
 * smallest. make smallest in fam.batteries?
 *)

open MapsSets
open Fam_batteries

module Point = struct
  type t = 
    {
      edge_id : int;
      distal : float;
    }

  let point_of_placement p = 
    {
      edge_id = Placement.location p;
      distal = Placement.distal_bl p;
    }
end

(* get the induced subtree, which is just a map from most distal occupied edge
 * numbers to the edge-location of their most distal placement as counted from
 * the proximal side of the edge. *)
let induced_of_placerun criterion pr = 
  let placem = Placerun.make_map_by_best_loc criterion pr in
  let check_edge i = (* look for placements on edge i *)
    if IntMap.mem i placem then
      match
        List.sort (* increasing, to get smallest distal bl *)
          compare
          (List.map Placement.distal_bl (IntMap.find i placem))
      with
      | hd::_ -> [i, 
                   (Gtree.get_bl i (Placerun.get_ref_tree pr)) -.
                     (Placement.distal_bl hd)]
      | [] -> assert(false)
    else
      []
  in
  IntMapFuns.of_pairlist
    (Stree.recur
      (fun i belowl ->
          let below = List.concat belowl in
          if below = [] then check_edge i
          else below)
      check_edge
      (Placerun.get_ref_tree pr))

(* returns if at least two of the descendants have things in them *)
let total_floatol =
  List.fold_left (fun x -> function | Some y -> x+.y | None -> x) 0.

(* compute the PD of an induced tree *)
let pd t ind = 
  let total = ref 0 in
  let add_to_tot x = total := x + !total in
  match
    Stree.recur
      (fun id below ->
        match List.filter ((<>) None) below with
        | [] -> IntMap.find_opt id ind (* perhaps start path *)
        | [Some x] -> Some ((Gtree.get_bl t id) +. x) (*continue path*)
        | _ as l -> 
            add_to_tot (total_floatol l); (* record lengths from distal paths *)
            Some (Gtree.get_bl t id)) (* start from mrca of those paths *)
      (fun id -> IntMap.find_opt id ind) (* perhaps start path *)
  with
  | Some _ -> !total
  | None -> failwith "empty induced tree"

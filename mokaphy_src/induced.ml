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
  let distal_spot_map = 
    IntMap.map
      (List.sort compare)
      (List.fold_right
        (fun pq ->
          let best = Pquery.best_place criterion pq in
          IntMapFuns.add_listly
            (Placement.location best)
            (Placement.distal_bl best))
        (Placerun.get_pqueries pr)
        IntMap.empty)
  in
  let check_edge i = 
    if IntMap.mem i distal_spot_map then
      [i, List.hd (IntMap.find i distal_spot_map)]
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
      (Gtree.get_stree (Placerun.get_ref_tree pr)))

(* returns if at least two of the descendants have things in them *)
let total_floatol =
  List.fold_left (fun x -> function | Some y -> x+.y | None -> x) 0.

(* compute the PD of an induced tree. we recursively go through the tree,
 * returning Some len if there is a placement distal to us, with len being the
 * length of the path to the last recorded MRCA. *)
let pd_of_induced t ind = 
  (* start recording the total branch length from the most distal placement *)
  let perhaps_start_path id =
    match IntMapFuns.opt_find id ind with
    | None -> None (* no path *)
    | Some x -> Some ((Gtree.get_bl t id) -. x)
  in
  let total = ref 0. in
  let add_to_tot x = total := x +. !total in
  match
    Gtree.recur
      (fun id below ->
        match List.filter ((<>) None) below with
        | [] -> perhaps_start_path id
        | [Some x] -> Some ((Gtree.get_bl t id) +. x) (* continue path *)
        | _ as l -> 
            add_to_tot (total_floatol l); (* record lengths from distal paths *)
            Some (Gtree.get_bl t id)) (* start recording from mrca of those paths *)
      perhaps_start_path
      t
  with
  | Some _ -> !total
  | None -> failwith "empty induced tree"


(* later have a lazy data cache with the induceds? *)
let pd_of_pr criterion pr = 
  pd_of_induced (Placerun.get_ref_tree pr) 
                (induced_of_placerun criterion pr)
  

  (*
let pdfrac_map_of_inda t inda = 
  take an edge number and sort the induced spots on it
  then process the spots on the edge recursively, by having a current location
  and a current bitstring, then giving the next location and the next bitstring.
  a global addmap
  

  pd_of_induced 
    (Placerun.get_ref_tree pr) 
    (induced_of_placerun criterion pr)
  *)

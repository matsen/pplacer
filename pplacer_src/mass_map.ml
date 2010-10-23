(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets


type weighting_choice = Weighted | Unweighted


(* indiv makes the weighting for a given edge as a list of (distal_bl, weight)
 * for each placement *)
module Indiv = struct

  (* we just return the top one if unweighted *)
  let place_list_of_pquery weighting criterion pquery = 
    match weighting with
    | Weighted -> Pquery.place_list pquery
    | Unweighted -> [ Pquery.best_place criterion pquery ]

(* return a list of (id, (distal_bl, mass)).
 * will raise Pquery.Unplaced_pquery if finds unplaced pqueries.
 *)
  let split_pquery_mass weighting criterion mass_per_pquery pq = 
    let pc = place_list_of_pquery weighting criterion pq in
    List.map2
      (fun place weight ->
        (Placement.location place,
        (Placement.distal_bl place, 
        mass_per_pquery *. weight)))
      pc
      (Base.normalized_prob (List.map criterion pc))

(* assume that the list of pqueries in have unit mass. split that mass up to
 * each of the pqueries, breaking it up by weighted placements if desired.
 *)
  let mass_list_of_pquery_list weighting criterion pql = 
    let mass_per_pquery = 1. /. (float_of_int (List.length pql)) in
    List.flatten
      (List.map
        (split_pquery_mass weighting criterion mass_per_pquery)
         pql)

  let mass_list_of_placerun weighting criterion pr = 
    try
      mass_list_of_pquery_list 
        weighting
        criterion
        (Placerun.get_pqueries pr)
    with 
    | Pquery.Unplaced_pquery s ->
      invalid_arg (s^" unplaced in "^(Placerun.get_name pr))

  let of_placerun weighting criterion pr = 
      (IntMapFuns.of_pairlist_listly 
        (mass_list_of_placerun weighting criterion pr))

(* sort the placements along a given edge according to their location on
 * the edge in an increasing manner. *)
  let sort m = 
    IntMap.map 
      (List.sort (fun (a1,_) (a2,_) -> compare a1 a2))
      m

  let ppr = 
    IntMapFuns.ppr_gen
      (fun ff l ->
        List.iter
          (fun (distal, mass) -> 
            Format.fprintf ff "@[{d = %g; m = %g}@]" distal mass)
          l)

end


(* By_edge just considers the weight per edge *)
module By_edge = struct

  let of_placerun weighting criterion pr = 
    IntMap.map
      (List.fold_left (fun tot (_,weight) -> tot +. weight) 0.)
      (Indiv.of_placerun weighting criterion pr)

  (* we add zeroes in where things are empty *)
  let fill_out_zeroes mass_map ref_tree = 
    let rec aux accu = function
      | loc::rest ->
        aux 
          (if IntMap.mem loc accu then accu
          else IntMap.add loc 0. accu)
          rest
      | [] -> accu
    in
    aux 
      mass_map 
      (Stree.node_ids (Gtree.get_stree ref_tree))

  let of_placerun_with_zeroes weighting criterion pr = 
    fill_out_zeroes 
      (of_placerun weighting criterion pr)
      (Placerun.get_ref_tree pr)

  let total_mass m = IntMap.fold (fun _ v -> ( +. ) v) m 0.

  let normalize_mass m = 
    let tot = total_mass m in
    IntMap.map (fun x -> x /. tot) m

end

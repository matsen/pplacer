(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets

type weighting_choice = Weighted | Unweighted


(* we just return the top one if unweighted *)
let place_list_of_pquery weighting criterion pquery = 
  match weighting with
  | Weighted -> Pquery.place_list pquery
  | Unweighted -> [ Pquery.best_place criterion pquery ]


(* Pre as in pre-mass-map *)
module Pre = struct

  type mass_unit = 
    {
      loc : int;
      distal_bl : float;
      mass : float;
    }

  (* list across mass for a given placement *)
  type mul = mass_unit list

  (* list across pqueries *)
  type t = mul list

  let mass_unit loc ~distal_bl ~mass = 
    {loc=loc; distal_bl=distal_bl; mass=mass}
  let distal_mass_unit loc mass = mass_unit loc ~distal_bl:0. ~mass

(* will raise Pquery.Unplaced_pquery if finds unplaced pqueries.  *)
  let mul_of_pquery weighting criterion mass_per_pquery pq = 
    let pc = place_list_of_pquery weighting criterion pq in
    List.map2
      (fun place weight ->
        {
          loc = Placement.location place;
          distal_bl = Placement.distal_bl place;
          mass = mass_per_pquery *. weight
        })
      pc
      (Base.normalized_prob (List.map criterion pc))

(* assume that the list of pqueries in have unit mass. split that mass up to
 * each of the pqueries, breaking it up by weighted placements if desired.
 *)
  let of_pquery_list weighting criterion pql = 
    let mass_per_pquery = 1. /. (float_of_int (List.length pql)) in
    List.map
      (mul_of_pquery weighting criterion mass_per_pquery)
       pql

  let of_placerun weighting criterion pr = 
    try
      of_pquery_list 
        weighting
        criterion
        (Placerun.get_pqueries pr)
    with 
    | Pquery.Unplaced_pquery s ->
      invalid_arg (s^" unplaced in "^(Placerun.get_name pr))

  let mul_total_mass = List.fold_left (fun x mu -> x +. mu.mass) 0. 

  let total_mass = List.fold_left (fun x mul -> x +. mul_total_mass mul) 0. 

  let normalize_mass pre = 
    let tot = total_mass pre in
    List.map (List.map (fun mu -> {mu with mass = mu.mass /. tot})) pre

end


(* indiv makes the weighting for a given edge as a list of (distal_bl, weight)
 * for each placement *)
module Indiv = struct

  type t = (float * float) IntMap.t

  let of_pre pmm = 
    List.fold_left
      (fun m' mul ->
        (List.fold_left 
          (fun m mu -> 
            IntMapFuns.add_listly mu.Pre.loc (mu.Pre.distal_bl, mu.Pre.mass) m)
          m'
          mul))
      IntMap.empty
      pmm

  let of_placerun weighting criterion pr = 
    of_pre (Pre.of_placerun weighting criterion pr)

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

  type t = float IntMap.t

  let of_indiv = 
    IntMap.map (List.fold_left (fun tot (_,weight) -> tot +. weight) 0.)

  (* SPEED 
   * 
   * a faster version would be like
   *
   let h = Hashtbl.create ((IntMapFuns.nkeys ti_imap)/3) in
    let addto ti x = Hashtbl.replace h ti (x+.(hashtbl_find_zero h ti)) in
    List.iter
    (fun pq ->
      List.iter
      (fun p -> addto (tax_id_of_place p) (criterion p))
        (Pquery.place_list pq))
    (Placerun.get_pqueries pr);
      Mass_map.By_edge.normalize_mass (IntMap.map (hashtbl_find_zero h) ti_imap)
   * *)
  let of_pre pre = of_indiv (Indiv.of_pre pre)

  let of_placerun weighting criterion pr = 
    of_indiv (Indiv.of_placerun weighting criterion pr)

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

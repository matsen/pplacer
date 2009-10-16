(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * calculate the distance between placeruns.
 *)

open Fam_batteries
open MapsSets
open Mokaphy_prefs

type weighting_choice = Weighted | Unweighted

(*
 * note: this requires that placerun1 and placerun2 don't have any unplaced
 * sequences.
 *)
let pair_dist criterion weighting p placerun1 placerun2 = 
  let context = 
    Printf.sprintf 
      "comparing %s with %s" 
      (Placerun.get_name placerun1)
      (Placerun.get_name placerun2)
  and ref_tree = Placerun.get_ref_tree placerun1
  in
  if ref_tree <> Placerun.get_ref_tree placerun2 then
    failwith ("reference trees not the same when "^context);
  let process_pcl pquery = 
    match weighting with
    | Weighted -> Pquery.place_list pquery
    | Unweighted -> [ Pquery.best_place criterion pquery ]
  in
  try
    let make_pcl placerun = 
      List.map
        process_pcl
        (Placerun.get_pqueries placerun)
    in
    Kr_distance.pcl_pair_distance 
      criterion 
      ref_tree 
      p
      (make_pcl placerun1)
      (make_pcl placerun2)
  with
  | Kr_distance.Invalid_place_loc a -> 
      invalid_arg
        (Printf.sprintf 
          "%g is not a valid placement location when %s" a context)
  | Pquery.Unplaced_pquery s ->
      invalid_arg (s^" unplaced when "^context)
  | Kr_distance.Total_kr_not_zero tkr ->
      failwith ("total kr_vect not zero for "^context^": "^(string_of_float tkr))

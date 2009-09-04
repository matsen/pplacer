(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)


(* placement.ml
 * 
 * care and feeding of placements.
 * terminology: placecoll (or pc)-- collection of placements for a given sequence
 * terminology: nplacecoll (or npc)-- named placecoll. (query seq name, place list)
 * terminology: npcl-- nplacecoll list
 * TODO: migrate from id_best_hash_of_placement_list to map version
 *)
open Fam_batteries
open MapsSets
open Stree


type placement = {location: int; 
                  ml_ratio : float;
                  post_prob : float option;
                  log_like: float;
                  marginal_prob: float option;
                  distal_bl: float; 
                  pendant_bl: float }

let location      p = p.location
let ml_ratio      p = p.ml_ratio
let post_prob     p = p.post_prob
let log_like      p = p.log_like
let marginal_prob p = p.marginal_prob
let distal_bl     p = p.distal_bl
let pendant_bl    p = p.pendant_bl

let compare_ml_place rp1 rp2 =
  compare rp1.ml_ratio rp2.ml_ratio

let compare_pp_place rp1 rp2 =
  match (rp1.post_prob, rp2.post_prob) with
  | (Some f1, Some f2) -> compare f1 f2
  | (_, _) -> failwith "compare_pp_place : not enough PP!"

let ml_filter_placement_list cutoff placements = 
  List.filter (fun ml_place -> ml_place.ml_ratio > cutoff) placements


(*
 * make a hash of all of the placements, keyed by the id of their placement node
 * NOTE: filter is only applied to the one with the highest cmp
 * not very good design
 * *)
let id_best_hash_of_placement_list cmp filter named_place_list =
  let h = Hashtbl.create (List.length named_place_list / 2) in
  List.iter (
    fun (name, placements) -> 
      if placements <> [] then begin
        let best_place = 
          List.hd 
            (List.sort (* want decreasing sort *)
              (fun x y -> - (cmp x y))
              placements) in
        if filter best_place then
          Hashtbl.add h (best_place.location) (name, best_place)
      end
  ) named_place_list;
  h

(* decreasing sort of a placecoll
 *)
let sort_placecoll cmp pc =
  List.sort (fun x y -> - (cmp x y)) pc

let add_to_list_intmap k v m = 
  if IntMap.mem k m then
    IntMap.add k (v::(IntMap.find k m)) m
  else 
    IntMap.add k [v] m

(* make a map 
 * (best location for nplacecoll) -> list of nplacecoll at that loc
 *)
let sorted_npcl_map_by_best_loc_of_npc_list cmp npc_list =
  List.fold_right 
    (fun (name, pc) (placed_map, unplaced_list) ->
      match sort_placecoll cmp pc with
      | best::_ as sorted ->
          (add_to_list_intmap best.location (name,sorted) placed_map,
          unplaced_list)
      | [] -> 
          (placed_map, name::unplaced_list))
    npc_list
    (IntMap.empty, [])


let by_name_map_of_place_hash place_hash = 
  Hashtbl.fold (
    fun _ (name, place) name_map ->
      if StringMap.mem name name_map then
        failwith(name^" appears multiply in name_map!")
      else
        StringMap.add name place name_map
  ) place_hash StringMap.empty


let make_ml_ratio_filter cutoff placement = 
  placement.ml_ratio > cutoff

let make_post_prob_filter cutoff placement = 
  match placement.post_prob with
  | Some x -> x > cutoff
  | None -> assert(false)


(* to and from strings *)

let opt_to_str fmt = function
  | Some x -> Printf.sprintf fmt x
  | None -> "-"

let float_opt_of_string s = 
  if s = "-" then None
  else Some (float_of_string s)

let placement_to_str place = 
  Printf.sprintf "%d\t%8f\t%s\t%8g\t%s\t%8g\t%8g" 
  place.location
  place.ml_ratio
  (opt_to_str "%8f" place.post_prob)
  place.log_like
  (opt_to_str "%8g" place.marginal_prob)
  place.distal_bl
  place.pendant_bl
 
let placement_of_str str = 
  let strs = Array.of_list (Str.split (Str.regexp "[ \t]+") str) in
  if Array.length strs <> 7 then 
    failwith ("placement_of_str : wrong number of entries in "^str)
  else
    {location = int_of_string strs.(0);
    ml_ratio = float_of_string strs.(1);
    post_prob = float_opt_of_string strs.(2);
    log_like = float_of_string strs.(3);
    marginal_prob = float_opt_of_string strs.(4);
    distal_bl = float_of_string strs.(5);
    pendant_bl = float_of_string strs.(6)}


    (*
let x = 
  placement_of_str 
  "3       0.000000        -       -1329.78     -        0       0.855013"
*)

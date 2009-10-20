(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 * 
 * care and feeding of placements.
 *
 *)
open Fam_batteries
open MapsSets
open Stree

exception No_PP

let get_pp = function
  | Some pp -> pp
  | None -> raise No_PP

type placement = {location: int; 
                  ml_ratio : float;
                  post_prob : float option;
                  log_like: float;
                  marginal_prob: float option;
                  distal_bl: float; 
                  pendant_bl: float }

let location          p = p.location
let ml_ratio          p = p.ml_ratio
let post_prob_opt     p = p.post_prob
let post_prob         p = get_pp p.post_prob
let log_like          p = p.log_like
let marginal_prob_opt p = p.marginal_prob
let marginal_prob     p = get_pp p.marginal_prob
let distal_bl         p = p.distal_bl
let pendant_bl        p = p.pendant_bl

let make_ml loc ~ml_ratio ~log_like ~dist_bl ~pend_bl =
  {location      =  loc;
  ml_ratio       =  ml_ratio;
  log_like       =  log_like;
  distal_bl      =  dist_bl;
  pendant_bl     =  pend_bl;
  marginal_prob  =  None;
  post_prob      =  None}

let add_pp p ~marginal_prob ~post_prob = 
  {p with 
    post_prob      =  Some post_prob;
    marginal_prob  =  Some marginal_prob}

let compare_placements criterion rp1 rp2 =
  compare (criterion rp1) (criterion rp2)

let sort_placecoll criterion pc =
  List.sort (fun x y -> - (compare_placements criterion) x y) pc

let filter_placecoll criterion cutoff pc =
  List.filter (fun p -> criterion p > cutoff) pc

(* decreasing sort of a placecoll
 *)
let add_to_list_intmap k v m = 
  if IntMap.mem k m then
    IntMap.add k (v::(IntMap.find k m)) m
  else 
    IntMap.add k [v] m

(* make (m,l), where m is a map
 * (best location for nplacecoll) -> list of nplacecoll at that loc
 * and l is a list of unplaced sequences.
 *)
let sorted_npcl_map_by_best_loc_of_npc_list criterion npc_list =
  List.fold_right
    (fun (name, pc) (unplaced_list, placed_map) ->
      match sort_placecoll criterion pc with
      | best::_ as sorted ->
          (unplaced_list,
          add_to_list_intmap best.location (name,sorted) placed_map)
      | [] ->
          (name::unplaced_list, placed_map))
    npc_list
    ([], IntMap.empty)

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

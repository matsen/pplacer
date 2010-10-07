(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 * 
 * care and feeding of placements.
 *
 *)

open Fam_batteries
open MapsSets
open Stree

exception No_PP
exception No_classif

let get_some except = function
  | Some pp -> pp
  | None -> raise except

type placement = 
  {
    location      : int; 
    ml_ratio      : float;
    post_prob     : float option;
    log_like      : float;
    marginal_prob : float option;
    distal_bl     : float; 
    pendant_bl    : float;
    classif       : Tax_id.tax_id option
  }

let location          p = p.location
let ml_ratio          p = p.ml_ratio
let post_prob_opt     p = p.post_prob
let post_prob         p = get_some No_PP p.post_prob
let log_like          p = p.log_like
let marginal_prob_opt p = p.marginal_prob
let marginal_prob     p = get_some No_PP p.marginal_prob
let distal_bl         p = p.distal_bl
let pendant_bl        p = p.pendant_bl
let classif_opt       p = p.classif
let classif           p = get_some No_classif p.classif

let make_ml loc ~ml_ratio ~log_like ~dist_bl ~pend_bl =
  {location      =  loc;
  ml_ratio       =  ml_ratio;
  log_like       =  log_like;
  distal_bl      =  dist_bl;
  pendant_bl     =  pend_bl;
  marginal_prob  =  None;
  post_prob      =  None;
  classif        =  None;}

let add_pp p ~marginal_prob ~post_prob = 
  {p with 
    post_prob      =  Some post_prob;
    marginal_prob  =  Some marginal_prob}

let add_classif p c = {p with classif = Some c}

let compare_placements criterion rp1 rp2 =
  compare (criterion rp1) (criterion rp2)

let sort_placecoll criterion pc =
  List.sort (fun x y -> - (compare_placements criterion) x y) pc

let filter_place_list criterion cutoff pc =
  List.filter (fun p -> criterion p > cutoff) pc

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

let opt_to_str f = function
  | Some x -> f x
  | None -> "-"

let float_opt_of_string s = 
  if s = "-" then None
  else Some (float_of_string s)

let placement_to_str place = 
  Printf.sprintf "%d\t%8f\t%s\t%8g\t%s\t%8g\t%8g\t%s" 
  place.location
  place.ml_ratio
  (opt_to_str (Printf.sprintf "%8f") place.post_prob)
  place.log_like
  (opt_to_str (Printf.sprintf "%8f") place.marginal_prob)
  place.distal_bl
  place.pendant_bl
  (opt_to_str Tax_id.to_string place.classif)
 
let placement_of_str str = 
  let strs = Array.of_list (Str.split (Str.regexp "[ \t]+") str) in
  if Array.length strs <> 8 then 
    failwith ("placement_of_str : wrong number of entries in "^str)
  else
    {
      location       =  int_of_string        strs.(0);
      ml_ratio       =  float_of_string      strs.(1);
      post_prob      =  float_opt_of_string  strs.(2);
      log_like       =  float_of_string      strs.(3);
      marginal_prob  =  float_opt_of_string  strs.(4);
      distal_bl      =  float_of_string      strs.(5);
      pendant_bl     =  float_of_string      strs.(6);
      classif        =  Some (Tax_id.of_string strs.(7));
    }

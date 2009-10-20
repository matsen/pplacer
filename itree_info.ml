(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * the information attached to strees to make itrees.
 * *)


open MapsSets
open Fam_batteries

type itree_info = 
  {taxon : string IntMap.t;
   boot : float IntMap.t;
   bl : float IntMap.t}

let empty_info = 
  {taxon = IntMap.empty;
   boot = IntMap.empty;
   bl = IntMap.empty}

let get_something_opt entrymap info id = 
  try Some (IntMap.find id (entrymap info)) with
  | Not_found -> None

let get_taxon_opt = get_something_opt (fun info -> info.taxon)
let get_boot_opt = get_something_opt (fun info -> info.boot)
let get_bl_opt = get_something_opt (fun info -> info.bl)

let get_something get_it entryname info id = 
  match get_it info id with
  | Some x -> x
  | None -> 
      invalid_arg (Printf.sprintf "%s %d not found" entryname id)

let get_taxon = get_something get_taxon_opt "taxon"
let get_boot = get_something get_boot_opt "bootstrap"
let get_bl = get_something get_bl_opt "branch length"

(* here, taxon, boot, and bl must be specified as 'a options. *)
let add_info vert_num ~taxon ~boot ~bl prev_info = 
  {taxon = IntMapFuns.opt_add vert_num taxon prev_info.taxon;
  boot = IntMapFuns.opt_add vert_num boot prev_info.boot;
  bl = IntMapFuns.opt_add vert_num bl prev_info.bl}

(* here, they don't have to be specified, but when they are they should be 'a *)
let opt_add_info vert_num ?taxon ?boot ?bl prev_info = 
  add_info vert_num ~taxon ~boot ~bl prev_info

let combine_node_infos n1 n2 = 
  {taxon = IntMapFuns.union n1.taxon n2.taxon;
   boot = IntMapFuns.union n1.boot n2.boot;
   bl = IntMapFuns.union n1.bl n2.bl}

(* increase all of the keys in the info map by "by" *)
let boost by info = 
  let boost_map m = 
    IntMap.fold (fun k v -> IntMap.add (k+by) v) m IntMap.empty in
  { taxon = boost_map info.taxon;
  boot = boost_map info.boot;
  bl = boost_map info.bl }

let copy_info ~dest ~src id = 
  add_info 
    id 
    ~taxon:(get_taxon_opt src id)
    ~boot:(get_boot_opt src id)
    ~bl:(get_bl_opt src id)
    dest



(* IO *)

let gstring_of_float x = Printf.sprintf "%g" x

(*
Unquoted labels may not contain blanks, parentheses, square brackets,
single_quotes, colons, semicolons, or commas.
Underscore characters in unquoted labels are converted to blanks.
*)
let need_quoting = 
  Array.to_list (StringFuns.to_char_array " ()[]':;,")

let if_quote_label label = 
  try
    String.iter 
      (fun c -> if List.mem c need_quoting then raise Exit)
      label;
    false
  with
  | Exit -> true

let prepare_label label = 
  if if_quote_label label then "'"^label^"'" else label


(* entry_to_string :
 * this will produce funny output if there is bootstrap info for a taxon (which
 * is nonsensical)
 * *)
let entry_to_string ni i = 
  let entryToStr toStrFn map = 
    if IntMap.mem i map then toStrFn (IntMap.find i map)
    else ""
  in
  (entryToStr prepare_label ni.taxon)^
  (entryToStr gstring_of_float ni.boot)^(
    if IntMap.mem i ni.bl then (":"^(gstring_of_float (IntMap.find i ni.bl)))
    else "")

(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * a placerun is a data structure representing a single pplacer run.
*)

open Fam_batteries
open MapsSets

type placerun = 
  {
    (* listed in increasing order of going-to-be-changed *)
    ref_tree  :  Itree.itree;
    prefs     :  Prefs.prefs;
    name      :  string;
    pqueries  :  Pquery.pquery list;
  }

let make ref_tree prefs name pqueries = 
  {
    ref_tree  =  ref_tree;
    prefs     =  prefs;
    name      =  name;
    pqueries  =  pqueries;
  }

let get_ref_tree p = p.ref_tree
let get_prefs p = p.prefs
let get_name p = p.name
let get_pqueries p = p.pqueries

let set_ref_tree p ref_tree = {p with ref_tree = ref_tree}
let set_prefs p prefs = {p with prefs = prefs}
let set_name p name = {p with name = name}
let set_pqueries p pqueries = {p with pqueries = pqueries}

let n_pqueries p = List.length p.pqueries
let make_map_by_best_loc criterion pr =
  Pquery.make_map_by_best_loc criterion (get_pqueries pr)

let contains_unplaced_queries p =
  try
    List.iter
      (fun pquery ->
        if not (Pquery.is_placed pquery) then raise Exit)
      (get_pqueries p);
    false
  with
  | Exit -> true

let get_same get_thing thing_name pr1 pr2 = 
  let x = get_thing pr1 in
  if x = get_thing pr2 then x
  else 
    failwith 
      (Printf.sprintf
        "%ss for %s and %s not the same! Were these run with the same reference tree and model parameters (e.g. statistics files?)"
        thing_name (get_name pr1) (get_name pr2))

let combine name pr1 pr2 = 
  let ref_tree = get_same get_ref_tree "Reference tree" pr1 pr2 in
  let prefs = get_same get_prefs "Pref" pr1 pr2 in
  make
    ref_tree
    prefs
    name
    ((get_pqueries pr1) @ (get_pqueries pr2))

let warn_about_duplicate_names placerun = 
  let name_set = StringSet.empty in
  let _ = 
    List.fold_left
      (fun accu pquery ->
        let name = Pquery.name pquery in
        if StringSet.mem name accu then
          Printf.printf "Warning: query name %s appears multiple times.\n" name;
        StringSet.add name accu)
      name_set
      (get_pqueries placerun)
  in
  ()

(* for each entry of a (name, f) list, make a placerun with the given name and
 * the pqueries that satisfy f *)
let multifilter named_f_list placerun = 
  let ref_tree = get_ref_tree placerun
  and prefs = get_prefs placerun in
  List.map2
    (make ref_tree prefs)
    (List.map fst named_f_list)
    (ListFuns.multifilter
      (List.map snd named_f_list)
      (get_pqueries placerun))


let cutoff_str x = Printf.sprintf "%1.2g" x
(* let cutoff_str x = Printf.sprintf "%02d" (Base.round (100. *. x)) *)

let cutoff_filter make_name cutoff_fun =
  multifilter 
    [ (make_name "lt"), (fun pq -> not (cutoff_fun pq));
      (make_name "ge"), cutoff_fun ]

(* split up placeruns by ml ratio *)
let partition_by_ml ml_cutoff placerun = 
  let make_name which_str = 
    (get_name placerun)^".L"^which_str^(cutoff_str ml_cutoff) in
  let geq_cutoff pq = 
    match Pquery.opt_best_place Placement.ml_ratio pq with
    | Some p -> ml_cutoff <= Placement.ml_ratio p
    | None -> false
  in
  cutoff_filter make_name geq_cutoff placerun

(* split up placeruns by bounce distance *)
let partition_by_bounce bounce_cutoff placerun = 
  let t = get_ref_tree placerun in
  let make_name which_str = 
    (get_name placerun)^".B"^which_str^(cutoff_str bounce_cutoff)
  and tree_length = Itree.tree_length t in
  let geq_cutoff pq = 
    (Bounce.raw_bounce_of_pquery t pq) /. tree_length >= 
      bounce_cutoff
  in
  cutoff_filter make_name geq_cutoff placerun

let re_matches rex s = Str.string_match rex s 0

let warn_about_multiple_matches rex_list placerun = 
  List.iter
    (fun s -> 
      Printf.printf "Warning: multiple match on %s\n" s)
    (Base.find_multiple_matches
      (List.map re_matches rex_list)
      (List.map Pquery.name (get_pqueries placerun)))

let multifilter_by_regex named_regex_list placerun = 
  multifilter 
    (List.map 
      (fun (name, rex) -> 
        (name, fun pq -> re_matches rex (Pquery.name pq)))
      named_regex_list)
    placerun


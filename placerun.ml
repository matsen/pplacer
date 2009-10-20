(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * a placerun is a data structure representing a single pplacer run.
*)

open Fam_batteries
open MapsSets

type placerun = 
  {
    ref_tree  :  Itree.itree;
    name      :  string;
    pqueries  :  Pquery.pquery list;
    (* put in the preferences ? *)
  }

let make ref_tree name pqueries = 
  {
    ref_tree  =  ref_tree;
    name      =  name;
    pqueries  =  pqueries;
  }

let get_ref_tree p = p.ref_tree
let get_name p = p.name
let get_pqueries p = p.pqueries

let set_ref_tree p ref_tree = {p with ref_tree = ref_tree}
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

let combine name p1 p2 = 
  let ref_tree = get_ref_tree p1 in
  if ref_tree <> get_ref_tree p2 then
    failwith 
      (Printf.sprintf
        "Reference trees for %s and %s not the same!"
        (get_name p1)
        (get_name p2));
   make
     ref_tree
     name
     ((get_pqueries p1) @ (get_pqueries p2))

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
  let ref_tree = get_ref_tree placerun in
  List.map2
    (make ref_tree)
    (List.map fst named_f_list)
    (ListFuns.multifilter
      (List.map snd named_f_list)
      (get_pqueries placerun))

(* split up placeruns by ml ratio *)
let partition_by_ml ml_cutoff placerun = 
  let cutoff_str = 
    Printf.sprintf "%02d" (int_of_float (100. *. ml_cutoff)) in
  let make_name which_str = 
    (get_name placerun)^".L"^which_str^cutoff_str in
  let geq_ml_cutoff cutoff pq = 
    match Pquery.opt_best_place Placement.ml_ratio pq with
    | Some p -> cutoff <= Placement.ml_ratio p
    | None -> false
  in
  multifilter 
    [ (make_name "lt"), (fun pq -> not (geq_ml_cutoff ml_cutoff pq));
      (make_name "ge"), (geq_ml_cutoff ml_cutoff)]
    placerun

let re_matches rex s = Str.string_match rex s 0

(*
let warn_about_multiple_matches rex_list strings = 
  List.iter
    (fun s -> 
      Printf.printf "Warning: multiple match on %s\n" s)
    (Base.find_multiple_matches
      (List.map re_matches rex_list)
      strings)
*)

let multifilter_by_regex named_regex_list placerun = 
  multifilter 
    (List.map 
      (fun (name, rex) -> 
        (name, fun pq -> re_matches rex (Pquery.name pq)))
      named_regex_list)
    placerun


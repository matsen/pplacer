(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.e
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 * 
 * the actual functionality of placeutil
 *)

open MapsSets
open Fam_batteries


let write_placeutil_preamble argv ch =
  Printf.fprintf ch 
    "# made by placeutil run as: %s\n" 
    (String.concat " " (Array.to_list argv))

(* returns (below, above), where below are the pqueries whose best placement is
 * non existent or doesn't satisfy the cutoff, and above are the pqueries that
 * do. *)
let partition_by_cutoff criterion cutoff pquery_list = 
  List.partition 
    (fun pq ->
      match Pquery.opt_best_place criterion pq with
      | Some place -> criterion place < cutoff
      | None -> false)
    pquery_list


(* re splitting *)
let re_split_rex = Str.regexp "\\(.*\\)[ \t]+\"\\(.*\\)\"" 

let read_re_split_file fname = 
  List.map 
    (fun line -> 
      if Str.string_match re_split_rex line 0 then
        ((Str.matched_group 1 line),
        Str.regexp (Str.matched_group 2 line))
      else
        failwith(
          Printf.sprintf 
            "The following line of %s could not be read as a split regex: %s"
            fname
            line))
    (File_parsing.filter_comments 
      (File_parsing.filter_empty_lines 
        (File_parsing.string_list_of_file fname)))

let write_bounce_list ch pr = 
  let t = Placerun.get_ref_tree pr in
  let tl = Itree.tree_length t in
  String_matrix.write_padded ch
    (Array.map
      (fun pq ->
        [| 
          string_of_float ((Bounce.raw_bounce_of_pquery t pq) /. tl);
          Pquery.name pq; 
        |]) 
      (Array.of_list (Placerun.get_pqueries pr)))

let print_bounce_list = write_bounce_list stdout

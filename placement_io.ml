(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Fam_batteries
open MapsSets
open Placement

 
(* ***** WRITING ***** *)

let write_npc ch (name, places) = 
  Printf.fprintf ch ">%s\n" name;
  List.iter 
    (fun p -> Printf.fprintf ch "%s\n" (placement_to_str p)) 
    places

let write_placements_arr ch placement_arr = 
  Array.iter (write_npc ch) placement_arr

let ml_sort_placement_list pl =
  List.sort (fun x y -> - compare_ml_place x y) pl

let pp_sort_placement_list pl =
  List.sort (fun x y -> - compare_pp_place x y) pl
 
let write_best_of_placement_arr ch use_pp results = 
  write_placements_arr ch (
    Array.map (
      fun (name, places) ->
        assert(places <> []);
        let best_ml = List.hd (ml_sort_placement_list places) in
        (name, 
        best_ml::(
          if use_pp then begin
            let best_pp = List.hd (pp_sort_placement_list places) in
            if best_pp = best_ml then [] (* we already have that placement *)
            else [best_pp]
          end
          else []))
    ) results)

let write_npcl_map ch m =
  IntMap.iter
    (fun loc npcl ->
      Printf.fprintf ch "# location %d\n" loc;
      List.iter (write_npc ch) npcl)
    m


(* ***** READING ***** *)

(* returns (ref_tree, nplacecoll list)
 * conventions for placement files: 
  * first line is 
# pplacer [version] run ...
  * then whatever. last line before placements is
# reference tree: [ref tre]
*)
let parse_place_file version_str place_fname = 
  if not (Filename.check_suffix place_fname ".place") then
    failwith("Pplacer place file names must end in .place, unlike "^place_fname);
  let reftree_rex = Str.regexp "^# reference tree: \\(.*\\)"
  and fastaname_rex = Str.regexp "^>"
  and str_match rex str = Str.string_match rex str 0
  in
  match 
  (* split up the file by the fastanames *)
    File_parsing.partition_list 
      (str_match fastaname_rex) 
      (File_parsing.string_list_of_file place_fname) 
  with
  | header::placements -> begin
  try 
  (* parse the header, getting a ref tree *)
  let ref_tree = 
    match header with
    | version_line::header_tl -> begin
    (* make sure we have appropriate versions *)
    Scanf.sscanf version_line "# pplacer %s run" 
      (fun file_vers ->
        if file_vers <> version_str then
          failwith "incompatible versions of placeviz and pplacer!");
    (* get the ref tree *)
      try
        let tree_line,_ = 
          File_parsing.find_beginning 
            (str_match reftree_rex) 
            header_tl 
        in
        Stree_io.of_newick_str (Str.matched_group 1 tree_line)
      with | Not_found -> failwith "couldn't find ref tree line!"
    end
    | [] -> failwith (place_fname^" no header!")
    in
    (ref_tree,
    (* now parse the placements *)
    List.map 
      (function
      | name::places ->
        (Alignment.read_fasta_name name, 
        List.map Placement.placement_of_str places)
      | [] -> assert(false))
      (List.map File_parsing.filter_comments placements))
  with
  | Scanf.Scan_failure s ->
      failwith ("problem with the place file: "^s)
  end
  | [] -> failwith (place_fname^" empty place file!")

(* returns an nplacecoll list.
 * conventions for placement files: 
  * first line is 
# pplacer [version] run ...
  * then whatever. last line before placements is
# reference tree: [ref tre]
*)
let parse_place_filex version_str place_fname = 
  if not (Filename.check_suffix place_fname ".place") then
    failwith("Pplacer place file names must end in .place, unlike "^place_fname);
  let in_ch = open_in place_fname
  and reftree_rex = Str.regexp "^# reference tree"
  and fastaname_rex = Str.regexp "^>"
  in
  try 
    Scanf.sscanf (input_line in_ch) "# pplacer %s run" (
      fun file_vers ->
        if file_vers <> version_str then
          failwith "incompatible versions of placeviz and pplacer!"
    );
    let rec get_tree () =
      try
        let line = input_line in_ch in
        if Str.string_match reftree_rex line 0 then 
          Scanf.sscanf 
            line "# reference tree: %s" Stree_io.of_newick_str
        else get_tree () 
      with | End_of_file -> failwith "couldn't find ref tree line!"
      in
    let ref_tree = get_tree () in
    let rec get_placements placement_list curr_name curr_placements = 
      let process_placements () = 
        if curr_placements <> [] then
          (curr_name, List.rev curr_placements)::placement_list
        else placement_list
      in
      try
        let line = input_line in_ch in
        if Str.string_match fastaname_rex line 0 then begin
          (* we have hit a new set of placements *)
          get_placements 
            (process_placements ())
            (Alignment.read_fasta_name line)
            []
        end
        else begin
          get_placements placement_list curr_name 
            ((Placement.placement_of_str line)::curr_placements)
        end
      with 
      | End_of_file -> (process_placements ())
    in
    let places = List.rev (get_placements [] "" []) in
    close_in in_ch;
    (ref_tree, places)
  with
  | Scanf.Scan_failure s ->
      failwith ("problem with the place file: "^s)
 

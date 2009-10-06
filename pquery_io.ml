(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Fam_batteries
open MapsSets


let chop_place_extension fname =
  if Filename.check_suffix fname ".place" then
    Filename.chop_extension fname
  else 
    invalid_arg ("this program requires place files ending with .place suffix")

 
(* ***** WRITING ***** *)

let write ch pq = 
  Printf.fprintf ch ">%s\n" (Pquery.name pq);
  Printf.fprintf ch "%s\n" (Pquery.seq pq);
  List.iter 
    (fun p -> 
      Printf.fprintf ch "%s\n" (Placement.placement_to_str p)) 
    (Pquery.place_list pq)

let write_unplaced ch unplaced_list = 
  if unplaced_list <> [] then
    Printf.fprintf ch "# unplaced sequences\n";
  List.iter (write ch) unplaced_list

let write_placed_map ch placed_map = 
  IntMap.iter
    (fun loc npcl ->
      Printf.fprintf ch "# location %d\n" loc;
      List.iter (write ch) npcl)
    placed_map

let write_by_best_loc criterion ch pq_list =
  let (unplaced_l, placed_map) = 
    Pquery.make_map_by_best_loc criterion pq_list in
  write_unplaced ch unplaced_l;
  write_placed_map ch placed_map


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
      | name::seq::places ->
          Pquery.make_ml_sorted
            ~name:(Alignment.read_fasta_name name)
            ~seq
            (List.map Placement.placement_of_str places)
      | _ -> 
          invalid_arg "problem with place file. missing sequence data?")
      (List.map File_parsing.filter_comments placements))
  with
  | Scanf.Scan_failure s ->
      failwith ("problem with the place file: "^s)
  end
  | [] -> failwith (place_fname^" empty place file!")


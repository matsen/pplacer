(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Fam_batteries
open MapsSets

let compatible_versions = [ "v0.3"; "v1.0"; "v1.1"; ]

let bifurcation_warning = 
  "Warning: pplacer results make the most sense when the \
  given tree is multifurcating at the root. See manual for details."

let chop_place_extension fname =
  if Filename.check_suffix fname ".place" then
    Filename.chop_extension fname
  else 
    invalid_arg ("this program requires place files ending with .place suffix, unlike "^fname)

(* ***** WRITING ***** *)

let output_fmt_str = "# output format: location, ML weight ratio, PP, ML likelihood, marginal likelihood, attachment location (distal length), pendant branch length, containment classification, classification"

let write_unplaced ch unplaced_list = 
  if unplaced_list <> [] then
    Printf.fprintf ch "# unplaced sequences\n";
  List.iter (Pquery_io.write ch) unplaced_list

let write_placed_map ch placed_map = 
  IntMap.iter
    (fun loc npcl ->
      Printf.fprintf ch "# location %d\n" loc;
      List.iter (Pquery_io.write ch) npcl)
    placed_map

let write_by_best_loc criterion ch placerun =
  let (unplaced_l, placed_map) = 
    Placerun.make_map_by_best_loc criterion placerun in
  write_unplaced ch unplaced_l;
  write_placed_map ch placed_map

let pre_fname out_dir pr = out_dir^"/"^(Placerun.get_name pr)

let to_file invocation out_dir placerun = 
  Placerun.warn_about_duplicate_names placerun;
  let ch = 
    open_out ((pre_fname out_dir placerun)^".place") in
  let ref_tree = Placerun.get_ref_tree placerun in
  Printf.fprintf ch "# pplacer %s run, %s\n"        
    Version.version_revision (Base.date_time_str ());
  Printf.fprintf ch "# invocation: %s\n" invocation;
  Prefs.write ch (Placerun.get_prefs placerun);
  Printf.fprintf ch "%s\n" output_fmt_str;
  if not (Stree.multifurcating_at_root (Gtree.get_stree ref_tree)) then
    Printf.fprintf ch "# %s\n" bifurcation_warning;
  (* we do the following to write a tree with the node numbers in place of
   * the bootstrap values, and at @ at the end of the taxon names *)
  Printf.fprintf ch "# numbered reference tree: %s\n" 
    (Newick.to_string (Newick.to_numbered ref_tree));
  Printf.fprintf ch "# reference tree: %s\n" (Newick.to_string ref_tree);
  write_by_best_loc 
    Placement.ml_ratio 
    ch 
    placerun;
  close_out ch


(* ***** READING ***** *)

(* read the header, i.e. the first set of lines in the placefile *)
let prefs_and_rt_of_header hlines = 
  let reftree_rex = Str.regexp "^# reference tree: \\(.*\\)"
  and invocation_rex = Str.regexp "^# invocation:"
  and str_match rex str = Str.string_match rex str 0
  in
  try 
    match hlines with
    | [] -> failwith ("Place file missing header!")
    | version_line::header_tl -> begin
      (* make sure we have appropriate versions *)
      Scanf.sscanf version_line "# pplacer %s run" 
        (fun file_vers ->
          if not (List.mem 
                   (Version.chop_revision file_vers)
                   compatible_versions) then
            failwith
              (Printf.sprintf 
               "This file is from version %s which is incompatible with the present version of %s"
               file_vers
               Version.version));
      let _, post_invocation = 
        File_parsing.find_beginning 
          (str_match invocation_rex) 
          header_tl in
      let prefs = Prefs.read post_invocation in
      (* get the ref tree *)
      let tree_line,_ = 
        File_parsing.find_beginning 
          (str_match reftree_rex) 
          post_invocation 
      in
      (prefs,
        Newick.of_string (Str.matched_group 1 tree_line))
    end
  with
  | Scanf.Scan_failure s ->
    failwith ("problem with the place file: "^s)
  | Not_found -> failwith "couldn't find ref tree line!"


(* returns a placerun
 * conventions for placement files: 
  * first line is 
# pplacer [version] run ...
  * then whatever. last line before placements is
# reference tree: [ref tre]
*)
let of_file ?load_seq:(load_seq=true) place_fname = 
  let fastaname_rex = Str.regexp "^>"
  and ch = open_in place_fname 
  in
  let next_batch () = 
    File_parsing.read_lines_until ch fastaname_rex 
  in 
  let (prefs, ref_tree) = 
    try prefs_and_rt_of_header (next_batch ()) with
    | End_of_file -> failwith (place_fname^" empty place file!")
  in
  let rec get_pqueries accu =
    try
      get_pqueries 
        ((Pquery_io.parse_pquery 
          ~load_seq
          (File_parsing.filter_comments (next_batch ())))::accu)
    with
    | End_of_file -> List.rev accu
  in
  (* parse the header, getting a ref tree *)
  Placerun.make
    ref_tree
    prefs
    (chop_place_extension (Filename.basename place_fname))
    (get_pqueries [])


(* *** CSV CSV CSV CSV CSV CSV CSV CSV *** *)

let csv_output_fmt_str = 
  R_csv.strl_to_str
    (List.map R_csv.quote
      [
        "name";
        "hit";
        "location";
        "ml_ratio";
        "post_prob";
        "log_like";
        "marginal_prob";
        "distal_bl";
        "pendant_bl";
        "contain_classif";
        "classif"
        ])
 
let write_csv ch pr = 
  Printf.fprintf ch "%s\n" csv_output_fmt_str;
  List.iter (Pquery_io.write_csv ch) (Placerun.get_pqueries pr)

let to_csv_file out_dir pr = 
  let ch = open_out ((pre_fname out_dir pr)^".place.csv") in
  write_csv ch pr;
  close_out ch

let ppr_placerun ff pr =
  Format.fprintf ff "Placerun %s" pr.Placerun.name


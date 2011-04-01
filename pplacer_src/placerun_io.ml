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

let to_file invocation out_fname placerun =
  Placerun.warn_about_duplicate_names placerun;
  let ch = open_out out_fname
  and ref_tree = Placerun.get_ref_tree placerun
  in
  Printf.fprintf ch "# pplacer %s run, %s\n"
    Version.version_revision (Base.date_time_str ());
  Printf.fprintf ch "# invocation: %s\n" invocation;
  Printf.fprintf ch "%s\n" output_fmt_str;
  if not (Stree.multifurcating_at_root (Gtree.get_stree ref_tree)) then
    Printf.fprintf ch "# %s\n" bifurcation_warning;
  (* we do the following to write a tree with the node numbers in place of
   * the bootstrap values, and at @ at the end of the taxon names *)
  Printf.fprintf ch "# numbered reference tree: %s\n"
    (Newick_gtree.to_string (Newick_gtree.to_numbered ref_tree));
  Printf.fprintf ch "# reference tree: %s\n" (Newick_gtree.to_string ref_tree);
  write_by_best_loc
    Placement.ml_ratio
    ch
    placerun;
  close_out ch

let to_json_file invocation out_fname placerun =
  let ret = Hashtbl.create 8
  and meta = Hashtbl.create 16
  and ref_tree = Placerun.get_ref_tree placerun
  and pqueries = Placerun.get_pqueries placerun in
  Hashtbl.add meta "invocation" (Jsontype.String invocation);
  Hashtbl.add ret "metadata" (Jsontype.Object meta);

  let json_state = ref None in
  Hashtbl.add ret "placements" (Jsontype.Array (List.map (Pquery_io.to_json json_state) pqueries));
  Hashtbl.add ret "fields" (Jsontype.Array (List.map (fun s -> Jsontype.String s) (
    ["edge_num"; "likelihood"; "like_weight_ratio"; "distal_length"; "pendant_length"]
    @ begin match !json_state with
      | None
      | Some (false, false) -> []
      | Some (has_post_prob, has_classif) ->
        begin if has_post_prob then ["post_prob"; "marginal_prob"] else [] end
        @ begin if has_classif then ["classification"] else [] end
    end
  )));
  Hashtbl.add ret "tree" (Jsontype.String (Newick_gtree.to_string ~with_edge_labels:true ref_tree));
  Hashtbl.add ret "version" (Jsontype.Int 1);
  Json.to_file out_fname (Jsontype.Object ret)

(* ***** READING ***** *)

(* read the header, i.e. the first set of lines in the placefile *)
let rt_of_header hlines =
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
      (* get the ref tree *)
      let tree_line,_ =
        File_parsing.find_beginning
          (str_match reftree_rex)
          post_invocation
      in Newick_gtree.of_string (Str.matched_group 1 tree_line)
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
  let ref_tree =
    try rt_of_header (next_batch ()) with
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
    (chop_place_extension (Filename.basename place_fname))
    (get_pqueries [])



let of_json_file fname =
  let json = Jsontype.obj (Json.of_file fname) in
  let fields = List.map Jsontype.string (Jsontype.array (Hashtbl.find json "fields")) in
  let pql = List.map (Pquery_io.of_json fields) (Jsontype.array (Hashtbl.find json "placements")) in
  let ref_tree = Newick_gtree.of_string (Jsontype.string (Hashtbl.find json "tree")) in
  Placerun.make
    ref_tree
    (Filename.chop_extension (Filename.basename fname))
    pql

(* *** CSV CSV CSV CSV CSV CSV CSV CSV *** *)

let csv_col_names =
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
    "classif";
  ]

let to_csv_strl pr =
  List.flatten (List.map Pquery_io.to_csv_strl pr.Placerun.pqueries)

let to_csv_file out_fname pr =
  Csv.save out_fname (csv_col_names::(to_csv_strl pr))

let ppr_placerun ff pr =
  Format.fprintf ff "Placerun %s" pr.Placerun.name

let of_any_file fname =
  if Filename.check_suffix fname ".place" then
    of_file fname
  else if Filename.check_suffix fname ".json" then
    of_json_file fname
  else
    failwith ("unfamiliar suffix on " ^ fname)

let filtered_of_file ?verbose:(verbose=true) fname =
  Placerun.filter_unplaced ~verbose (of_any_file fname)

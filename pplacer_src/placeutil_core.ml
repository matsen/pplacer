(* the actual functionality of placeutil
 *)

open MapsSets
open Fam_batteries


let write_placeutil_preamble argv ch =
  Printf.fprintf ch
    "# made by placeutil run as: %s\n"
    (String.concat " " (Array.to_list argv))

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

    (*
(* split up placeruns *)
let partition_by_cutoff infix_str criterion use_edpl cutoff placerun =
  let t = Placerun.get_ref_tree placerun in
  (* which_str will be lt or ge *)
  let make_name which_str =
    (Placerun.get_name placerun)
    ^infix_str
    ^which_str
    ^(Placerun.cutoff_str cutoff)
  in
  let geq_cutoff =
    if use_edpl then begin
      (* calc the tree length only once to save time *)
      let tree_length = Gtree.tree_length t in
      fun pq ->
        ((Edpl.raw_edpl_of_pquery criterion t pq) /. tree_length >=
          cutoff)
    end
    else
      fun pq ->
        match Pquery.opt_best_place criterion pq with
        | Some p -> cutoff <= criterion p
        | None -> false
  in
  Placerun.cutoff_filter make_name geq_cutoff placerun

let write_edpl_list criterion ch pr =
  let t = Placerun.get_ref_tree pr in
  let tl = Gtree.tree_length t in
  String_matrix.write_padded ch
    (Array.map
      (fun pq ->
        [|
          string_of_float
            ((Edpl.raw_edpl_of_pquery criterion t pq) /. tl);
          Pquery.name pq;
        |])
      (Array.of_list (Placerun.get_pqueries pr)))
  *)
class outprefix_cmd () =
object
  val out_prefix = Subcommand.flag "-o"
    (Subcommand.Needs_argument ("out-prefix", "Set the prefix to write to. Required."))

  method specl = [
    Subcommand.string_flag out_prefix;
  ]
end


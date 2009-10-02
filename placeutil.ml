(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 *
 *)

open Fam_batteries
open MapsSets
open Placement

let version_str = "v0.2"
let verbose = false

let out_prefix = ref ""
let verbose = ref false
let ml_cutoff = ref 0.

let bifurcation_warning = 
  "Warning: pplacer results make the most sense when the \
  given tree is multifurcating at the root. See manual for details."

let parse_args () =
  let files  = ref [] in
  let out_prefix_opt = "-o", Arg.Set_string out_prefix,
    "Set the output prefix. Required if there are two or more input files."
  and verbose_opt = "-v", Arg.Set verbose,
    "Verbose output."
  and ml_cutoff_opt = "-l", Arg.Set_float ml_cutoff,
    "ML filtration cutoff."
  in
  let usage =
    "placeutil "^version_str
      ^"\nplaceutil ex1.place ex2.place ... combines place files, filters, then splits them back up again if you want.\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = [out_prefix_opt; verbose_opt; ml_cutoff_opt] in
  Arg.parse args anon_arg usage;
  List.rev !files

    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then begin
    let fnames = parse_args () in
    if fnames = [] then exit 0;
    let parsed = 
      List.map 
        (fun fname -> Placement_io.parse_place_file version_str fname)
        fnames
    in
    let out_prefix_complete = 
      (if !out_prefix <> "" then !out_prefix
      else if List.length fnames > 1 then 
        failwith "Please supply an out prefix with the -o option. This is required when there are two or more input files."
      else 
        (* hd: have already checked that fnames isn't [] *)
        Placement_io.chop_place_extension (List.hd fnames))
      ^(if !ml_cutoff = 0. then ""
      else Printf.sprintf ".L%02d" (int_of_float (100. *. !ml_cutoff))) in
    if !verbose then begin
      print_endline "combining placements...";
      List.iter2
        (fun (_, places) fname ->
          Printf.printf 
            "found %d placements in %s\n" 
            (List.length places)
            fname)
        parsed
        fnames
    end;
    if parsed = [] then exit 0;
    let ref_tree = 
      Base.complete_fold_left
        (fun prev_ref a_ref ->
          if prev_ref <> a_ref then
            failwith "Reference trees not all the same!";
          prev_ref)
        (List.map fst parsed)
    in
    let placement_out_ch = open_out (out_prefix_complete^".place")
    in
    Printf.fprintf placement_out_ch "# pplacer %s run\n" version_str;
    Printf.fprintf placement_out_ch 
               "# made by placeutil run as: %s\n" 
               (String.concat " " (Array.to_list Sys.argv));
    Printf.fprintf placement_out_ch "# output format: ML weight ratio, PP, ML likelihood, marginal likelihood, attachment location (distal length), pendant branch length\n";
    if not (Stree.multifurcating_at_root ref_tree.Stree.tree) then
      Printf.fprintf placement_out_ch "# %s\n" bifurcation_warning;
    Printf.fprintf placement_out_ch "# numbered reference tree: %s\n"
      (Stree_io.to_newick_numbered ref_tree);
    Printf.fprintf placement_out_ch "# reference tree: %s\n" (Stree_io.to_newick ref_tree);
    let combined = List.flatten (List.map snd parsed) in

    (*
    if !verbose then
      Printf.printf "writing %d placements\n" (List.length combined);

 *
 *        (* print some statistics *)
        Printf.printf "in %s, %d of %d made it through filter\n"
          place_fname
          (List.length (HashtblFuns.keys best_place_hash))
          (List.length named_places);
          *)

    let (unplaced_list, placed_map) = 
      Placement_io.write_npcl_sorted 
      placement_out_ch 
      Placement.ml_ratio
      combined
    in
    close_out placement_out_ch;

    try
      match 
        List.map 
          (fun placement_fname -> 
            (* filter out the annotations, which are zero length seqs *)
            Alignment.filter_zero_length
              (Alignment.read_align 
                (Str.replace_first
                  (Str.regexp ".place$")
                  ".loc.fasta"
                  placement_fname)))
          fnames
      with
      | h::t ->
          Placement_io.write_fasta_by_placement_loc 
            (out_prefix_complete^".loc.fasta")
            (List.fold_left Array.append h t)
            (* (List.fold_left Alignment.stack h t) *)
            unplaced_list 
            placed_map
      | [] -> ()
    with
    | Sys_error s -> 
        print_endline ("couldn't find some loc.fasta files, so will not be combining them. ("^s^")")
  end

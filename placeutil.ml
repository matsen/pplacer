(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * what if we just have placements in a big list, and split the list?
 * then we could read all of the loc.fasta files into a map.
 *)

open Fam_batteries
open MapsSets

let ml_cutoff_off_val = -.max_float
let re_sep_fname_off_val = ""

let out_prefix = ref ""
let verbose = ref false
let ml_cutoff = ref ml_cutoff_off_val
let re_sep_fname = ref re_sep_fname_off_val
let warn_multiple = ref true

let is_on opt_value off_val = !opt_value <> off_val

let parse_args () =
  let files  = ref [] in
  let out_prefix_opt = "-o", Arg.Set_string out_prefix,
    "Set the output prefix. Required if there are two or more input files."
  and verbose_opt = "-v", Arg.Set verbose,
    "Verbose output."
  and ml_cutoff_opt = "-l", Arg.Set_float ml_cutoff,
    "ML separation cutoff."
  and re_sep_fname_opt = "--reSepFile", Arg.Set_string re_sep_fname,
    "File name for the regular expression separation file."
  and warn_multiple_opt = "--noWarnMultipleRe", Arg.Clear warn_multiple,
    "Warn if a read name matches several regular expressions."
  in
  let usage =
    "placeutil "^Placerun_io.version_str
      ^"\nplaceutil ex1.place ex2.place ... combines place files, filters, then splits them back up again if you want.\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = [out_prefix_opt; verbose_opt; ml_cutoff_opt; re_sep_fname_opt; warn_multiple_opt] in
  Arg.parse args anon_arg usage;
  if is_on ml_cutoff ml_cutoff_off_val && !ml_cutoff < 0. then
    failwith "negative cutoff value?";
  List.rev !files

    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then begin
    let fnames = parse_args () in
    if fnames = [] then exit 0;
    (* parse the placements *)
    let parsed = 
      List.map 
        (fun fname -> Placerun_io.parse_place_file fname)
        fnames
    in
    if !verbose then begin
      print_endline "combining placements...";
      List.iter2
        (fun placerun fname ->
          Printf.printf 
            "found %d placements in %s\n" 
            (Placerun.n_pqueries placerun)
            fname)
        parsed
        fnames
    end;
    if parsed = [] then exit 0;
    (* make the out prefix *)
    let out_prefix_complete = 
      (if !out_prefix <> "" then !out_prefix
      else if List.length fnames > 1 then 
        failwith "Please supply an out prefix with the -o option. This is required when there are two or more input files."
      else 
        (* hd: have already checked that fnames isn't [] *)
        Placerun_io.chop_place_extension (List.hd fnames))
    in
    let combined = 
      ListFuns.complete_fold_left 
        (Placerun.combine out_prefix_complete) 
        parsed
    in
    let write_call ch = 
      Printf.fprintf ch "# made by placeutil run as: %s\n"
        (String.concat " " (Array.to_list Sys.argv))
    in
    let re_split_list = 
      if is_on re_sep_fname re_sep_fname_off_val then
        Placeutil_core.read_re_split_file (!re_sep_fname)
      else
        []
    in
    if List.length re_split_list = 1 then
      failwith "I only found one regular expression split. If you don't want to split by regular expression, just don't use the option.";
    (* let _ = Distance_mat.of_place_file (List.hd fnames) in *)
    (* ways to split *)
    let split_by_ml placerun = 
      if is_on ml_cutoff ml_cutoff_off_val then
        (Placerun.partition_by_ml (!ml_cutoff) placerun)
      else [placerun]
    in 
    let split_by_re placerun = 
      if re_split_list <> [] then
        (Placerun.multifilter_by_regex re_split_list placerun)
      else
        [placerun]
    in
    (* splitting *)
    let flat_split split_fun placerun_list = 
      List.flatten ((List.map split_fun) placerun_list)
    in
    let placerun_list = 
      List.fold_right
        (fun f a -> f a)
        (List.map flat_split [split_by_ml; split_by_re])
        [combined]
    in
    (* "main" *)
    if List.length fnames <= 1 && List.length placerun_list <= 1 then
        print_endline "hmm... I am not combining any files, and I don't have to split up the data in any way, so i'm not doing anything."
    else 
      List.iter (Placerun_io.to_file write_call) placerun_list
    end

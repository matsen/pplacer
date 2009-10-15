(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * what if we just have placements in a big list, and split the list?
 * then we could read all of the loc.fasta files into a map.
 *)

open Fam_batteries
open MapsSets

let out_prefix = ref ""
let verbose = ref false
let ml_cutoff = ref 0.
let re_sep_fname = ref ""
let warn_multiple = ref true

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
    (* function to split up the queries by likelihood ratio and then write them *)
    let process_pqueries placerun = 
      if !ml_cutoff <> 0. then begin
        List.iter
          (Placerun_io.to_file
            (fun ch ->
              write_call ch;
              Printf.fprintf ch 
                             "# ml split from %s" 
                             (Placerun.get_name placerun)))
          (Placerun.partition_by_ml 
            (!ml_cutoff)
            placerun)
      end
      else
        Placerun_io.to_file
          (fun ch -> write_call ch)
          placerun
    in 
    (* "main" *)
    if !re_sep_fname = "" then
      if List.length fnames > 1 || !ml_cutoff <> 0. then
        process_pqueries combined
      else 
        print_endline "hmm... I don't have to split up by the ML ratio cutoff or regular expressions, and I am not combining any files. so i'm not doing anything."
    else begin
      let re_split_list = 
        Placeutil_core.read_re_split_file (!re_sep_fname) in
      if List.length re_split_list <= 1 then
        failwith "I only found one regular expression split. If you don't want to split by regular expression, just don't use the option."
      else
        List.iter
          process_pqueries
          (Placerun.multifilter_by_regex 
            re_split_list
            combined)
    end
  end

(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 *)

open Fam_batteries
open MapsSets

let cutoff_off_val = -.max_float
let re_sep_fname_off_val = ""

let is_on opt_value off_val = !opt_value <> off_val

let out_prefix = ref ""
let verbose = ref false
let use_pp = ref false
let use_edpl = ref false
let cutoff = ref cutoff_off_val
let re_sep_fname = ref re_sep_fname_off_val
let warn_multiple = ref true
let print_edpl = ref false
let edge_distance_mat = ref false
let list_classify_refpkg = ref ""
let tax_exclude_fname = ref ""
let nboot = ref 0

let parse_args () = 
  let files  = ref [] in
  let args = 
   [
     "-o", Arg.Set_string out_prefix,
     "Set the output prefix. Required if there are two or more input files.";
     "-v", Arg.Set verbose,
     "Verbose output.";
     "-p", Arg.Set use_pp,
     "Use posterior probability for our criteria.";
     "-c", Arg.Set_float cutoff,
     "Specify separation cutoff value. Perform cutoff if this is set to some value.";
     "--edpl", Arg.Set use_edpl,
     "Use the EDPL for cutoff.";
     "--reSepFile", Arg.Set_string re_sep_fname,
     "File name for the regular expression separation file.";
     "--noWarnMultipleRe", Arg.Clear warn_multiple,
     "Don't warn if a read name matches several regular expressions.";
     "--printEDPL", Arg.Set print_edpl,
     "Print out a table of edpl values for each placement.";
     "--distmat", Arg.Set edge_distance_mat,
     "Print out a pairwise distance matrix between the edges.";
     "--list-classify-refpkg", Arg.Set_string list_classify_refpkg,
     "Classify a placerun using the designated refpkg in a way designed to go into SQL.";
     "--tax-exclude", Arg.Set_string tax_exclude_fname,
     "Supply a file (one taxid per line) which specifies taxids to exclude from the placefile.";
     "--boot", Arg.Set_int nboot,
     "Specify the number of bootstrapped placeruns to create.";
   ]
  in
  let usage =
    "placeutil "^Version.version_revision
      ^"\nplaceutil ex1.place ex2.place ... combines place files and (given some options) splits them back up again.\n"
  and anon_arg arg =
    files := arg :: !files in
  Arg.parse args anon_arg usage;
  if is_on cutoff cutoff_off_val && (!cutoff) < 0. then
    failwith "negative cutoff value?";
  List.rev !files

let () =
  if not !Sys.interactive then begin
    let fnames = parse_args () in
    if fnames = [] then exit 0;
    (* parse the placements *)
    let parsed = List.map Placerun_io.of_file fnames in
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
    let criterion = 
      if !use_pp then Placement.post_prob
      else Placement.ml_ratio
    in
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
    if !print_edpl then begin
      Placeutil_core.write_edpl_list criterion stdout combined
    end;
    let re_split_list = 
      if is_on re_sep_fname re_sep_fname_off_val then
        Placeutil_core.read_re_split_file (!re_sep_fname)
      else
        []
    in
    if List.length re_split_list = 1 then
      failwith "I only found one regular expression split. If you don't want to split by regular expression, just don't use the option.";
    (* ways to split *)
    let split_by_cutoff placerun = 
      if is_on cutoff cutoff_off_val then
        Placeutil_core.partition_by_cutoff 
          ((if !use_pp then ".PP" else ".ML")^
            (if !use_edpl then ".edpl" else "")^".")
          (if !use_pp then Placement.post_prob 
           else Placement.ml_ratio)
          (!use_edpl)
          (!cutoff)
          placerun
      else [placerun]
    in 
    let split_by_re placerun = 
      if re_split_list <> [] then begin
        if !warn_multiple then begin
          Placerun.warn_about_multiple_matches 
            (List.map snd re_split_list)
            placerun;
        end;
        Placerun.multifilter_by_regex re_split_list placerun
      end
      else
        [placerun]
    in
    let filter_by_taxids pr = 
      if !tax_exclude_fname <> "" then
        let removes = 
          Tax_id.TaxIdSetFuns.of_list
            (List.map Tax_id.of_string 
              (File_parsing.string_list_of_file (!tax_exclude_fname)))
        in
        Placerun.multifilter
          [(Placerun.get_name pr)^"."^(Filename.basename (!tax_exclude_fname)),
            (fun pq -> 
              not (Tax_id.TaxIdSet.mem
                    (Placement.contain_classif 
                      (Pquery.best_place criterion pq))
                    removes))]
          pr
      else
        [pr]
    in
    let boot pr =
      if !nboot <> 0 then ListFuns.init (!nboot) (Bootstrap.boot_placerun pr)
      else [pr]
    in
    (* splitting *)
    let flat_split split_fun placerun_list = 
      List.flatten ((List.map split_fun) placerun_list)
    in
    let placerun_list = 
      try
        List.fold_right
          (fun f a -> f a)
          (List.map 
            flat_split 
            [split_by_cutoff; split_by_re; filter_by_taxids; boot])
          [combined]
      with
      | Placement.No_PP -> failwith "Posterior probability use requested but some or all files were calculated without PP switched on."
    in
    (* "main" *)
    let invocation = String.concat " " (Array.to_list Sys.argv) in
    if List.length fnames <= 1 && 
       List.length placerun_list <= 1 && 
       not (!print_edpl) &&
       not (!edge_distance_mat) &&
       !list_classify_refpkg = "" &&
       !tax_exclude_fname = "" 
        then
      print_endline "hmm... I am not combining any files, and I don't have to split up the data in any way, so i'm not doing anything."
    else 
      List.iter (Placerun_io.to_file invocation ".") placerun_list;
    (* make edge-distance matrices *)
    if !edge_distance_mat then
      List.iter
       (fun pr ->
         let ch = open_out ((Placerun.get_name pr)^".distmat") in
         let ff = Format.formatter_of_out_channel ch in
         Uptri.ppr_lowtri ff 
           Edge_rdist.ppr_rdist
           (Edge_rdist.build_pairwise_dist 
           (Placerun.get_ref_tree pr));
         close_out ch)
       placerun_list;
    if !list_classify_refpkg <> "" then begin
      let rp = Refpkg.of_path (!list_classify_refpkg) in
      List_classify.classify Placement.contain_classif criterion rp parsed;
    end
  end

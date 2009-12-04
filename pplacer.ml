(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open Fam_batteries
open MapsSets
open Prefs


let parse_args () =
  let files  = ref [] 
  and prefs = Prefs.defaults ()
  in
  let usage =
    "pplacer "^Placerun_io.version_str^"\npplacer [options] -r ref_align -t ref_tree -s stats_file frags.fasta\n"
  and anon_arg arg =
    files := arg :: !files
  in
  Arg.parse (Prefs.args prefs) anon_arg usage;
  (List.rev !files, prefs)


    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then begin
    let (files, prefs) = parse_args () in 
    if files = [] then begin
      print_endline "Please specify some query sequences, or ask for an entropy tree."; 
      exit 0;
    end;
    if (verb_level prefs) >= 1 then 
      print_endline "Running pplacer analysis...";
    (* initialize the GSL error handler *)
    Gsl_error.init ();
    (* append on a slash to the dir if it's not there *)
    let ref_dir_complete = 
      match ref_dir prefs with
      | s when s = "" -> ""
      | s -> 
          if s.[(String.length s)-1] = '/' then s
          else s^"/"
    in
    (* load ref tree and alignment *)
    let ref_tree = match tree_fname prefs with
    | s when s = "" -> failwith "please specify a reference tree.";
    | s -> Newick.of_file (ref_dir_complete^s)
    and ref_align = match ref_align_fname prefs with
    | s when s = "" -> failwith "please specify a reference alignment."
    | s -> 
        Alignment.uppercase 
          (Alignment.read_align (ref_dir_complete^s))
    in
    if (verb_level prefs) > 0 && 
       not (Stree.multifurcating_at_root ref_tree.Gtree.stree) then
         print_endline Placerun_io.bifurcation_warning;
    if (verb_level prefs) > 1 then begin
      print_endline "found in reference alignment: ";
      Array.iter (
        fun (name,_) -> print_endline ("\t'"^name^"'")
      ) ref_align
    end;
    (* parse the phyml/raxml stat file if it exists and build model. we want the
     * command line arguments to override the settings in the stat file, so we
     * first parse things according to that file, then rerun the Arg.parser for
     * certain settings. *)
    let opt_freqs_transitions = match stats_fname prefs with
    | s when s = "" -> 
        Printf.printf
          "NOTE: you have not specified a stats file. I'm using the %s model.\n"
          (model_name prefs);
        None
    | _ -> Parse_stats.parse_stats ref_dir_complete prefs
    in
    (*
    (* now we re-parse the arguments to override if they are set *)
    Arg.parse_argv reparseable_opts (fun _ -> ()) "";
    disabled.
    *)
    (* build the model *)
    if Alignment_funs.is_nuc_align ref_align && (model_name prefs) <> "GTR" then
      failwith "You have given me what appears to be a nucleotide alignment, but have specified a model other than GTR. I only know GTR for nucleotides!";
    let model = 
      Model.build (model_name prefs) (emperical_freqs prefs)
                  opt_freqs_transitions ref_align 
                  (Gamma.discrete_gamma 
                    (gamma_n_cat prefs) (gamma_alpha prefs)) in
    (* find all the tree locations *)
    let all_locs = IntMapFuns.keys (Gtree.get_bark_map ref_tree) in
    assert(all_locs <> []);
    (* warning: only good if locations are as above. *)
    let locs = ListFuns.remove_last all_locs in
    if locs = [] then failwith("problem with reference tree: no placement locations.");
    let curr_time = Sys.time () in
    (* calculate like on ref tree *)
    if (verb_level prefs) >= 1 then begin
      print_string "Caching likelihood information on reference tree... ";
      flush_all ()
    end;
    let (dmap, pmap) = 
      Glv_int_map.dp_of_data model ref_align ref_tree locs in
    if (verb_level prefs) >= 1 then
      print_endline "done.";
    if (verb_level prefs) >= 2 then Printf.printf "tree like took\t%g\n" ((Sys.time ()) -. curr_time);
    let half_evolve_glv_map loc g = 
      Glv.evolve model g ((Gtree.get_bl ref_tree loc) /. 2.) in
    if (verb_level prefs) >= 1 then begin
      print_string "Preparing the edges for baseball... ";
      flush_all ();
    end;
    let halfd = IntMap.mapi half_evolve_glv_map dmap
    and halfp = IntMap.mapi half_evolve_glv_map pmap
    in
    if (verb_level prefs) >= 1 then begin
      print_endline "done."
    end;
    (* analyze query sequences *)
    let collect ret_code query_aln_fname =
      try
        let frc = 0 in
        let query_align = 
          Alignment.uppercase (Alignment.read_align query_aln_fname) in
        Alignment_funs.check_for_repeats (Alignment.getNameArr query_align);
        let query_bname = 
          Filename.basename (Filename.chop_extension query_aln_fname) in
        let prior = 
          if uniform_prior prefs then Core.Uniform_prior
          else Core.Exponential_prior (
            (* exponential with mean = average branch length *)
            (Gtree.tree_length ref_tree) /. 
              (float_of_int (Gtree.n_edges ref_tree))) 
        in
        let results = 
          Core.pplacer_core prefs query_bname prior
            model ref_align ref_tree query_align 
            ~dmap ~pmap ~halfd ~halfp locs in
        Placerun_io.to_file
          (String.concat " " (Array.to_list Sys.argv))
          (Placerun.make 
             ref_tree 
             prefs
             query_bname 
             (Array.to_list results));
        if frc = 0 && ret_code = 1 then 0 else ret_code
      with Sys_error msg -> prerr_endline msg; 2 in
    let retVal = List.fold_left collect 1 files in
    if verb_level prefs > 0 then Common_base.print_elapsed_time ();
    if verb_level prefs > 0 then Common_base.print_memory_usage ();
    exit retVal
  end

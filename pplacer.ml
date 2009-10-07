(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.

*)

open Fam_batteries
open MapsSets
open Prefs

let version_str = "v0.3"

let prefs = 
  { 
    (* basics *)
    tree_fname = ref "";
    ref_align_fname = ref "";
    stats_fname = ref "";
    (* tree calc *)
    start_pend = ref 0.5;
    max_pend = ref 2.;
    tolerance = ref 0.01;
    calc_pp = ref false;
    uniform_prior = ref false;
    pp_rel_err = ref 0.01;
    (* playing ball *)
    max_strikes = ref 6;
    strike_box = ref 3.;
    max_pitches = ref 40;
    (* model *)
    emperical_freqs = ref true;
    model_name = ref "LG";
    gamma_n_cat = ref 1;
    gamma_alpha = ref 1.;
    (* reading and writing *)
    verb_level = ref 1;
    write_masked = ref false;
    ratio_cutoff = ref 0.05;
    only_write_best = ref false
  }

let bifurcation_warning = 
  "Warning: pplacer results make the most sense when the \
  given tree is multifurcating at the root. See manual for details."

(* these are the args that we re-parse after parsing the phyml stat file *)
let model_name_opt = "-m", Arg.Set_string prefs.model_name,
  "Set the sequence substitution model. Protein options are LG (default) or WAG. \
  For nucleotides the GTR parameters must be specified via a stats file."
and gamma_n_cat_opt = "--gammaCats", Arg.Set_int prefs.gamma_n_cat,
 "Specify the number of categories for a discrete gamma model. (Default is \
 one, i.e. no gamma rate variation.)"
and gamma_alpha_opt = "--gammaAlpha", Arg.Set_float prefs.gamma_alpha,
 "Specify the shape parameter for a discrete gamma model."
let reparseable_opts = [model_name_opt; gamma_n_cat_opt; gamma_alpha_opt] 
let spec_with_default symbol setfun p help = 
  (symbol, setfun p, Printf.sprintf help !p)

let parse_args () =
  let files  = ref [] in
  let tree_fname_opt = "-t", Arg.Set_string prefs.tree_fname,
   "Specify the reference tree filename."
  and ref_align_fname_opt = "-r", Arg.Set_string prefs.ref_align_fname,
   "Specify the reference alignment filename."
  and verb_level_opt = spec_with_default "-v" (fun o -> Arg.Set_int o) prefs.verb_level 
          "Set verbosity level. 0 is silent, and 2 is quite a lot. Default is %d."
  and calc_pp_opt = "-p", Arg.Set prefs.calc_pp, 
  "Calculate posterior probabilities."
  and ratio_cutoff_opt = spec_with_default "--ratioCutoff" (fun o -> Arg.Set_float o) prefs.ratio_cutoff
   "Specify the ratio cutoff for recording in the .place file. Default is %g."
  and only_write_best_opt = "-b", Arg.Set prefs.only_write_best,
   "Only record the best PP and ML placements in the .place file, and do so \ 
   for every fragment."
  and stats_fname_opt = "-s", Arg.Set_string prefs.stats_fname,
   "Supply a phyml stats.txt file or a RAxML info file which specifies the model parameters. \
   The information in this file can be overriden on the command line."
  and max_pend_opt = spec_with_default "--maxPend" (fun o -> Arg.Set_float o) prefs.max_pend
   "Set the maximum pendant branch length for the ML and Bayes calculations. Default is %g."
  and tolerance_opt = spec_with_default "--mlTolerance" (fun o -> Arg.Set_float o) prefs.tolerance
   "Specify the tolerance for the branch length maximization. Default is %g."
  and rel_err_opt = spec_with_default "--ppRelErr" (fun o -> Arg.Set_float o) prefs.pp_rel_err
   "Specify the relative error for the posterior probability calculation. Default is %g."
  and model_freqs_opt = "--modelFreqs", Arg.Clear prefs.emperical_freqs,
   "Use protein frequencies counted from the chosen model rather than counts \
   from the reference alignment."
  and unif_prior_opt = "--uniformPrior", Arg.Set prefs.uniform_prior,
   "Use a uniform prior rather than exponential in the posterior probability \
   calculation."
  and write_masked_opt = "--writeMasked", Arg.Set prefs.write_masked,
   "Write out the reference alignment with the query sequence, masked to the \
   region without gaps in the query."
  and max_strikes_opt = spec_with_default "--maxStrikes" (fun o -> Arg.Set_int o) prefs.max_strikes
   "Set the maximum number of strikes for baseball. Setting to zero disables ball playing. Default is %d."
  and strike_box_opt = spec_with_default "--strikeBox" (fun o -> Arg.Set_float o) prefs.strike_box
   "Set the size of the strike box in log likelihood units. Default is %g."
  and max_pitches_opt = spec_with_default "--maxPitches" (fun o -> Arg.Set_int o) prefs.max_pitches
   "Set the maximum number of pitches for baseball. Default is %d."
  in
  let usage =
    "pplacer "^version_str^"\npplacer [options] -t ref_tree -r ref_align -s stats_file frags.fasta\n"
  and anon_arg arg =
    files := arg :: !files in
  let opts = 
    [
      tree_fname_opt; 
      ref_align_fname_opt; 
      stats_fname_opt;  
    ] 
    @ reparseable_opts @
    [
      calc_pp_opt; 
      unif_prior_opt; 
      model_freqs_opt; 
      max_strikes_opt; 
      strike_box_opt; 
      max_pitches_opt;
      max_pend_opt; 
      tolerance_opt; 
      rel_err_opt;
      ratio_cutoff_opt;
      verb_level_opt; 
      write_masked_opt; 
      only_write_best_opt; 
    ]
  in
  Arg.parse opts anon_arg usage;
  List.rev !files
     
    (* note return code of 0 is OK *)
let () =
  if not !Sys.interactive then begin
    if (verb_level prefs) >= 1 then 
      print_endline "Running pplacer analysis...";
    (* initialize the GSL error handler *)
    Gsl_error.init ();
    let files = parse_args () in 
    if files = [] then begin
      print_endline "please specify some query sequences."; 
      exit 0;
    end;
    (* load ref tree and alignment *)
    let ref_tree = match tree_fname prefs with
    | s when s = "" -> failwith "please specify a reference tree.";
    | s -> Stree_io.of_newick_file s
    and ref_align = match ref_align_fname prefs with
    | s when s = "" -> failwith "please specify a reference alignment."
    | s -> Alignment.uppercase (Alignment.read_align s)
    in
    if (verb_level prefs) > 0 && 
       not (Stree.multifurcating_at_root ref_tree.Stree.tree) then
         print_endline bifurcation_warning;
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
    | _ -> Parse_stats.parse_stats prefs
    in
    (*
    (* now we re-parse the arguments to override if they are set *)
    Arg.parse_argv reparseable_opts (fun _ -> ()) "";
    disabled.
    *)
    (* build the model *)
    if AlignmentFuns.is_nuc_align ref_align && (model_name prefs) <> "GTR" then
      failwith "You have given me what appears to be a nucleotide alignment, but have specified a model other than GTR. I only know GTR for nucleotides!";
    let model = 
      Model.build (model_name prefs) (emperical_freqs prefs)
                  opt_freqs_transitions ref_align 
                  (Gamma.discrete_gamma 
                    (gamma_n_cat prefs) (gamma_alpha prefs)) in
    (* find all the tree locations *)
    let all_locs = IntMapFuns.keys ref_tree.Stree.info.Stree.bl in
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
      GlvIntMap.dp_of_data model ref_align ref_tree locs in
    if (verb_level prefs) >= 1 then
      print_endline "done.";
    if (verb_level prefs) >= 2 then Printf.printf "tree like took\t%g\n" ((Sys.time ()) -. curr_time);

    (* analyze query sequences *)
    let collect ret_code query_aln_fname =
      try
        let frc = 0 in
        let query_align = 
          Alignment.uppercase (Alignment.read_align query_aln_fname) in
        AlignmentFuns.check_for_repeats (Alignment.getNameArr query_align);
        let query_bname = Filename.chop_extension query_aln_fname in
        let out_ch = 
          open_out (query_bname^".place") in
        Printf.fprintf out_ch "# pplacer %s run, %s\n"        
          version_str (Base.date_time_str ());
        Printf.fprintf out_ch 
          "# invocation: %s\n" (String.concat " " (Array.to_list Sys.argv));
        Prefs.write_prefs out_ch prefs;
        Printf.fprintf out_ch "# output format: location, ML weight ratio, PP, ML likelihood, marginal likelihood, attachment location (distal length), pendant branch length\n";
        if not (Stree.multifurcating_at_root ref_tree.Stree.tree) then
          Printf.fprintf out_ch "# %s\n" bifurcation_warning;
        Printf.fprintf out_ch "# numbered reference tree: %s\n"
        (* we do the following to write a tree with the node numbers in place of
         * the bootstrap values, and at @ at the end of the taxon names *)
          (Stree_io.to_newick (Stree_io.make_numbered_tree ref_tree));
        Printf.fprintf out_ch "# reference tree: %s\n" (Stree_io.to_newick ref_tree);
        let prior = 
          if uniform_prior prefs then Core.Uniform_prior
          else Core.Exponential_prior (
            (* exponential with mean = average branch length *)
            (Stree.tree_length ref_tree) /. 
              (float_of_int (Stree.n_edges ref_tree.Stree.tree))) 
        in
        let results = 
          Core.pplacer_core prefs prior
            model ref_align ref_tree query_align ~dmap ~pmap locs in
        Pquery_io.write_by_best_loc 
          Placement.ml_ratio 
          out_ch 
          (Array.to_list results);
        close_out out_ch;
        if frc = 0 && ret_code = 1 then 0 else ret_code
      with Sys_error msg -> prerr_endline msg; 2 in
    let retVal = List.fold_left collect 1 files in
    if verb_level prefs > 0 then Common_base.printElapsedTime ();
    exit retVal
  end

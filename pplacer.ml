(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
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
    "pplacer "^Version.version_revision^"\npplacer [options] -r ref_align -t ref_tree -s stats_file frags.fasta\n"
  and anon_arg arg =
    files := arg :: !files
  in
  Arg.parse (Prefs.args prefs) anon_arg usage;
  (List.rev !files, prefs)


let () =
  if not !Sys.interactive then begin
    let (files, prefs) = parse_args () in 
    Prefs.check prefs;
    if files = [] then begin
      print_endline "Please specify some query sequences."; 
      exit 0;
    end;
    if (verb_level prefs) >= 1 then 
      Printf.printf 
        "Running pplacer %s analysis...\n"
        Version.version_revision;
    (* initialize the GSL error handler *)
    Gsl_error.init ();
    (* check that the directories exist and get a good in path *)
    Check.directory (out_dir prefs);
    let ref_dir_complete =
      match ref_dir prefs with
      | s when s = "" -> ""
      | s ->
          Check.directory s;
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
    let opt_transitions = match stats_fname prefs with
    | s when s = "" -> 
        Printf.printf
          "NOTE: you have not specified a stats file. I'm using the %s model.\n"
          (model_name prefs);
        None
    | _ -> Parse_stats.parse_stats ref_dir_complete prefs
    in
    (* build the model *)
    if Alignment_funs.is_nuc_align ref_align && (model_name prefs) <> "GTR" then
      failwith "You have given me what appears to be a nucleotide alignment, but have specified a model other than GTR. I only know GTR for nucleotides!";
    let model = 
      Model.build (model_name prefs) (emperical_freqs prefs)
                  opt_transitions ref_align 
                  (Gamma.discrete_gamma 
                    (gamma_n_cat prefs) (gamma_alpha prefs)) in
    (* the like data from the alignment *)
    let like_aln_map = 
      Like_stree.like_aln_map_of_data 
       (Model.seq_type model) ref_align ref_tree 
    in
    (* pretending *)
    if pretend prefs then begin
      Check.pretend model ref_align files;
      print_endline "everything looks OK.";
      exit 0;
    end;
    (* find all the tree locations *)
    let all_locs = IntMapFuns.keys (Gtree.get_bark_map ref_tree) in
    assert(all_locs <> []);
    (* the last element in the list is the root, and we don't want to place there *)
    let locs = ListFuns.remove_last all_locs in
    if locs = [] then failwith("problem with reference tree: no placement locations.");
    let curr_time = Sys.time () in
    (* calculate like on ref tree *)
    if (verb_level prefs) >= 2 then
      Printf.printf "memory before reference tree calculation (gb): %g\n" (Memory.curr_gb ());
    if (verb_level prefs) >= 1 then begin
      print_string "Caching likelihood information on reference tree... ";
      flush_all ()
    end;
    (* allocate our memory *)
    let darr = Like_stree.glv_arr_for model ref_align ref_tree in
    let parr = Glv_arr.mimic darr
    and halfd = Glv_arr.mimic darr
    and halfp = Glv_arr.mimic darr
    in
    (* do the reference tree likelihood calculation. we do so using halfd and
     * one glv from halfp as our utility storage *)
    let util_glv = Glv_arr.get_one halfp in
    Like_stree.calc_distal_and_proximal model ref_tree like_aln_map 
      util_glv ~distal_glv_arr:darr ~proximal_glv_arr:parr 
      ~util_glv_arr:halfd;
    if (verb_level prefs) >= 1 then
      print_endline "done.";
    if (verb_level prefs) >= 2 then
      Printf.printf "memory after reference tree calculation (gb): %g\n" (Memory.curr_gb ());
    if (verb_level prefs) >= 2 then Printf.printf "tree like took\t%g\n" ((Sys.time ()) -. curr_time);
    (* pull exponents *)
    List.iter 
      (Glv_arr.iter (Glv.perhaps_pull_exponent (-10)))
      [darr; parr;];
    (* baseball calculation *)
    if (verb_level prefs) >= 1 then begin
      print_string "Preparing the edges for baseball... ";
      flush_all ();
    end;
    let half_bl_fun loc = (Gtree.get_bl ref_tree loc) /. 2. in
    Glv_arr.evolve_into model ~src:darr ~dst:halfd half_bl_fun;
    Glv_arr.evolve_into model ~src:parr ~dst:halfp half_bl_fun;
    if (verb_level prefs) >= 1 then begin
      print_endline "done."
    end;
    (* tree likelihood *)
    let zero_d = Glv_arr.get_one halfd
    and zero_p = Glv_arr.get_one halfp in
    let ones = Glv.mimic zero_d in
    Glv.set_all ones 0 1.;
    Printf.printf "tree likelihood is %g\n" 
                  (Glv.log_like3 model zero_p zero_d ones);
    (* analyze query sequences *)
    let mem_usage = ref 0. in
    List.iter 
      (fun query_fname ->
        let query_bname = 
          Filename.basename (Filename.chop_extension query_fname) in
        let prior = 
          if uniform_prior prefs then Core.Uniform_prior
          else Core.Exponential_prior 
            (* exponential with mean = average branch length *)
            ((Gtree.tree_length ref_tree) /. 
              (float_of_int (Gtree.n_edges ref_tree))) 
        in
        let results = 
          Core.pplacer_core mem_usage prefs query_fname prior
            model ref_align ref_tree
            ~darr ~parr ~halfd ~halfp locs in
        (* write output if we aren't in fantasy mode *)
        if fantasy prefs = 0. then
          Placerun_io.to_file
            (String.concat " " (Array.to_list Sys.argv))
            (out_dir prefs)
            (Placerun.make 
               ref_tree 
               prefs
               query_bname 
               (Array.to_list results)))
      files;
    (* print final info *)
    if verb_level prefs >= 1 then begin
      Common_base.print_elapsed_time ();
      Printf.printf "maximal observed memory usage (gb): %g\n" 
                    (!mem_usage);
      Common_base.print_n_compactions ();
    end;
    exit 0
  end


open Fam_batteries
open MapsSets

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
    (* *********************************************************
    if files = [] then begin
      print_endline "Please specify some query sequences.";
      exit 0;
    end;
    *)
    if (Prefs.verb_level prefs) >= 1 then
      Printf.printf
        "Running pplacer %s analysis...\n"
        Version.version_revision;
    (* initialize the GSL error handler *)
    Gsl_error.init ();
    (* check that the directories exist and get a good in path *)
    Check.directory (Prefs.out_dir prefs);
    let ref_dir_complete =
      match Prefs.ref_dir prefs with
      | "" -> ""
      | s -> begin
          Check.directory s;
          if s.[(String.length s)-1] = '/' then s
          else s^"/"
      end
    in
    let rp =
      Refpkg.of_strmap
        (List.fold_right
    (* only set if the option string is non empty.
     * override the contents of the reference package. *)
          (fun (k,v) m ->
            if v = "" then m
            else StringMap.add k (ref_dir_complete^v) m)
          [
            "tree_file", Prefs.tree_fname prefs;
            "aln_fasta", Prefs.ref_align_fname prefs;
            "tree_stats", Prefs.stats_fname prefs;
          ]
          (match Prefs.refpkg_path prefs with
          | "" ->
              StringMap.add "name"
                (Base.safe_chop_extension (Prefs.ref_align_fname prefs))
                StringMap.empty
          | path -> Refpkg_parse.strmap_of_path path))
    in
    let ref_tree  = Refpkg.get_ref_tree  rp
    and ref_align = Refpkg.get_aln_fasta rp
    and model     = Refpkg.get_model     rp
    in
    if (Prefs.verb_level prefs) > 0 &&
      not (Stree.multifurcating_at_root ref_tree.Gtree.stree) then
         print_endline Placerun_io.bifurcation_warning;
    if (Prefs.verb_level prefs) > 1 then begin
      print_endline "found in reference alignment: ";
      Array.iter (
        fun (name,_) -> print_endline ("\t'"^name^"'")
      ) ref_align
    end;
    (* the like data from the alignment *)
    let like_aln_map =
      Like_stree.like_aln_map_of_data
       (Model.seq_type model) ref_align ref_tree
    in
    (* pretending *)
    if Prefs.pretend prefs then begin
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
    if (Prefs.verb_level prefs) >= 1 then begin
      print_string "Caching likelihood information on reference tree... ";
      flush_all ()
    end;
    (* allocate our memory *)
    let darr = Like_stree.glv_arr_for model ref_align ref_tree in
    let parr = Glv_arr.mimic darr
    and snodes = Glv_arr.mimic darr
    in
    (* do the reference tree likelihood calculation. we do so using halfd and
     * one glv from halfp as our utility storage *)
    let util_glv = Glv.mimic (Glv_arr.get_one snodes) in
    Like_stree.calc_distal_and_proximal model ref_tree like_aln_map
      util_glv ~distal_glv_arr:darr ~proximal_glv_arr:parr
      ~util_glv_arr:snodes;
    if (Prefs.verb_level prefs) >= 1 then
      print_endline "done.";
    if (Prefs.verb_level prefs) >= 2 then Printf.printf "tree like took\t%g\n" ((Sys.time ()) -. curr_time);
    (* pull exponents *)
    if (Prefs.verb_level prefs) >= 1 then begin
      print_string "Pulling exponents... ";
      flush_all ();
    end;
    List.iter
      (Glv_arr.iter (Glv.perhaps_pull_exponent (-10)))
      [darr; parr;];
    print_endline "done.";
    (* baseball calculation *)
    let half_bl_fun loc = (Gtree.get_bl ref_tree loc) /. 2. in
    if (Prefs.verb_level prefs) >= 1 then begin
      print_string "Preparing the edges for baseball... ";
      flush_all ();
    end;
    Glv_arr.prep_supernodes model ~dst:snodes darr parr half_bl_fun;
    if (Prefs.verb_level prefs) >= 1 then print_endline "done.";
    (*
    (* check tree likelihood *)
    let zero_d = Glv_arr.get_one darr
    and zero_p = Glv_arr.get_one parr
    and sn = Glv_arr.get_one snodes
    in
    let util_d = Glv.mimic zero_d
    and util_p = Glv.mimic zero_p in
    Glv.evolve_into model ~src:zero_d ~dst:util_d (half_bl_fun 0);
    Glv.evolve_into model ~src:zero_p ~dst:util_p (half_bl_fun 0);
    let util = Glv.mimic zero_d in
    Glv.set_all util 0 1.;
    Printf.printf "tree likelihood is %g\n"
                  (Glv.log_like3 model util_d util_p util);
    Printf.printf "supernode likelihood is %g\n"
                  (Glv.logdot model sn util);
    *)
    (* analyze query sequences *)
    List.iter
      (fun query_fname ->
        let query_bname =
          Filename.basename (Filename.chop_extension query_fname) in
        let prior =
          if Prefs.uniform_prior prefs then Core.Uniform_prior
          else Core.Exponential_prior
            (* exponential with mean = average branch length *)
            ((Gtree.tree_length ref_tree) /.
              (float_of_int (Gtree.n_edges ref_tree)))
        in
        let pr =
          Placerun.make
            ref_tree
            prefs
            query_bname
            (Array.to_list
              (Core.pplacer_core prefs query_fname prior
                model ref_align ref_tree
                ~darr ~parr ~snodes locs))
        in
        (* write output if we aren't in fantasy mode *)
        if Prefs.fantasy prefs = 0. then begin
          let final_pr =
            if not (Refpkg.tax_equipped rp) then pr
            else Refpkg.contain_classify rp pr
          and out_prefix = (Prefs.out_dir prefs)^"/"^(Placerun.get_name pr)
          and invocation = (String.concat " " (Array.to_list Sys.argv))
          in
          Placerun_io.to_file
            invocation
            (out_prefix^".place")
            final_pr;
          if Prefs.csv prefs then
            Placerun_io.to_csv_file (out_prefix^".csv") final_pr;
          if Prefs.json prefs then
            Placerun_io.to_json_file
              invocation
              (out_prefix ^ ".json")
              final_pr;
        end)
      files;
    (* print final info *)
    if Prefs.verb_level prefs >= 1 then begin
      Common_base.print_elapsed_time ();
      Common_base.print_n_compactions ();
    end;
    exit 0
  end

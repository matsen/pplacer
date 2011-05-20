open Multiprocessing
open Fam_batteries
open MapsSets

exception Finished

class ['a, 'b] pplacer_process (f: 'a -> 'b) gotfunc nextfunc progressfunc =

  (* The pplacer process borrows a lot of its implementation from the the
   * reference implementation of Multiprocessing.map, because the core of
   * pplacer is a pure function that takes sequences and yields results. The
   * difference comes from being able to decide per-sequence whether or not it
   * should be sent to the child at all. *)
  let child_func rd wr =
    marshal wr Ready;
    let rec aux () =
      match begin
        try
          Some (Marshal.from_channel rd)
        with
          | End_of_file -> None
      end with
        | Some x ->
          marshal
            wr
            begin
              try
                Data (f x)
              with
                | exn -> Exception exn
            end;
          aux ()
        | None -> close_in rd; close_out wr
    in aux ()
  in

object (self)
  inherit ['b] process child_func as super

  method private push =
    match begin
      try
        Some (nextfunc ())
      with
        | Finished -> None
    end with
      | Some x -> marshal self#wr x
      | None -> self#close

  method obj_received = function
    | Ready -> self#push
    | Data x -> gotfunc x; self#push
    | Exception exn
    | Fatal_exception exn -> raise (Child_error exn)

  method progress_received = progressfunc
end

let run_file prefs query_fname =
  if (Prefs.verb_level prefs) >= 1 then
    Printf.printf
      "Running pplacer %s analysis on %s...\n"
      Version.version_revision
      query_fname;
  let ref_dir_complete =
    match Prefs.ref_dir prefs with
      | "" -> ""
      | s -> begin
        Check.directory s;
        if s.[(String.length s)-1] = '/' then s
        else s^"/"
      end
  in

  (* string map which represents the elements of the reference package; these
   * may be actually a reference package or specified on the command line. *)
  let rp_strmap =
    List.fold_right
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
        | path -> Refpkg_parse.strmap_of_path path)
  in

  let ref_tree =
    try Newick_gtree.of_file (StringMap.find "tree_file" rp_strmap) with
    | Not_found -> failwith "please specify a reference tree with -t or -c"
  in

  (* *** split the sequences into a ref_aln and a query_list *** *)
  let ref_name_list = Newick_gtree.get_name_list ref_tree in
  let ref_name_set = StringSet.of_list ref_name_list in
  if List.length ref_name_list <> StringSet.cardinal ref_name_set then
    failwith("Repeated names in reference tree!");
  let seq_list = Alignment_funs.upper_list_of_any_file query_fname in
  let ref_list, query_list =
    List.partition
      (fun (name,_) -> StringSet.mem name ref_name_set)
      seq_list
  in
  let ref_align =
    if ref_list = [] then begin
      if (Prefs.verb_level prefs) >= 1 then
        print_endline
          "Didn't find any reference sequences in given alignment file. \
          Using supplied reference alignment.";
      try
        Alignment_funs.upper_aln_of_any_file
          (StringMap.find "aln_fasta" rp_strmap)
      with
      | Not_found ->
          failwith
          "Please specify a reference alignment with -r or -c, or include all \
          reference sequences in the primary alignment."
    end
    else begin
      if (Prefs.verb_level prefs) >= 1 then
        print_endline
          "Found reference sequences in given alignment file. \
          Using those for reference alignment.";
      Alignment.uppercase (Array.of_list ref_list)
    end
  in
  if (Prefs.verb_level prefs) >= 2 then begin
    print_endline "found in reference alignment: ";
    Array.iter
      (fun (name,_) -> print_endline ("\t'"^name^"'"))
      ref_align
  end;
  let n_sites = Alignment.length ref_align in


  (* *** build reference package *** *)
  let rp = Refpkg.of_strmap ~ref_tree ~ref_align prefs rp_strmap in
  let model = Refpkg.get_model rp in
  if (Prefs.verb_level prefs) > 0 &&
    not (Stree.multifurcating_at_root ref_tree.Gtree.stree) then
       print_endline Placerun_io.bifurcation_warning;

  (* *** make the likelihood vectors *** *)
  (* the like data from the alignment *)
  let like_aln_map =
    Like_stree.like_aln_map_of_data
     (Model.seq_type model) ref_align ref_tree
  in
  (* pretending *)
  if Prefs.pretend prefs then begin
    Check.pretend model ref_align [query_fname];
    print_endline "everything looks OK.";
    exit 0;
  end;
  (* find all the tree locations *)
  let all_locs = IntMap.keys (Gtree.get_bark_map ref_tree) in
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
  let darr = Like_stree.glv_arr_for model ref_tree n_sites in
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

  (* *** check tree likelihood *** *)
  if Prefs.check_like prefs then begin
    let utilv_nsites = Gsl_vector.create n_sites in
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
                  (Glv.log_like3 utilv_nsites model util_d util_p util);
    Printf.printf "supernode likelihood is %g\n"
                  (Glv.logdot utilv_nsites sn util);
  end;

  (* *** analyze query sequences *** *)
  let query_bname =
    Filename.basename (Filename.chop_extension query_fname) in
  let prior =
    if Prefs.uniform_prior prefs then Core.Uniform_prior
    else Core.Exponential_prior
      (* exponential with mean = average branch length *)
      ((Gtree.tree_length ref_tree) /.
        (float_of_int (Gtree.n_edges ref_tree)))
  in
  let partial = Core.pplacer_core
    prefs locs prior model ref_align ref_tree ~darr ~parr ~snodes
  in
  let n_done = ref 0 in
  let queries = List.length query_list in
  let show_query query_name =
    incr n_done;
    Printf.printf "working on %s (%d/%d)...\n" query_name (!n_done) queries;
    flush_all ()
  in
  let progressfunc msg =
    if String.rcontains_from msg 0 '>' then begin
      let query_name = String.sub msg 1 ((String.length msg) - 1) in
      show_query query_name
    end else
      Printf.printf "%s\n" msg
  in
  let q = Queue.create () in
  List.iter
    (fun (name, seq) -> Queue.push (name, (String.uppercase seq)) q)
    query_list;

  let gotfunc, cachefunc, donefunc = if Prefs.fantasy prefs <> 0. then begin
    (* fantasy baseball *)
    let fantasy_mat =
      Fantasy.make_fantasy_matrix
        ~max_strike_box:(int_of_float (Prefs.strike_box prefs))
        ~max_strikes:(Prefs.max_strikes prefs)
    and fantasy_mod = Base.round (100. *. (Prefs.fantasy_frac prefs))
    and n_fantasies = ref 0
    in
    let gotfunc = function
      | Core.Fantasy f ->
        Fantasy.add_to_fantasy_matrix f fantasy_mat;
        incr n_fantasies
      | _ -> failwith "expected fantasy result"
    and cachefunc _ =
      (* if cachefunc returns true, the current thing is skipped; modulo
       * division will be 0 for every N things *)
      let res = (!n_done) mod fantasy_mod <> 0 in
      incr n_done;
      res
    and donefunc () =
      Fantasy.results_to_file
        (Filename.basename (Filename.chop_extension query_fname))
        fantasy_mat (!n_fantasies);
      Fantasy.print_optimum fantasy_mat (Prefs.fantasy prefs) (!n_fantasies);
    in gotfunc, cachefunc, donefunc

  end else begin
    (* not fantasy baseball *)
    let query_tbl = Hashtbl.create 1024 in
    let gotfunc = function
      | Core.Pquery (seq, pq) -> Hashtbl.add query_tbl seq pq
      | _ -> failwith "expected pquery result"
    and cachefunc (name, seq) =
      match begin
        try
          Some (Hashtbl.find query_tbl seq)
        with
          | Not_found -> None
      end with
        | Some pq ->
          let pq = Pquery.set_namel pq (name :: pq.Pquery.namel) in
          incr n_done;
          Hashtbl.replace query_tbl seq pq;
          true
        | None -> false
    and donefunc () =
      let results = Hashtbl.fold
        (fun _ pq l -> pq :: l)
        query_tbl
        []
      in
      let pr =
        Placerun.make
          ref_tree
          query_bname
          results
      in
      let final_pr =
        if not (Refpkg.tax_equipped rp) then pr
        else Refpkg.classify rp pr
      and out_prefix = (Prefs.out_dir prefs)^"/"^(Placerun.get_name pr)
      and invocation = (String.concat " " (Array.to_list Sys.argv))
      in
      Placerun_io.to_json_file
        invocation
        (out_prefix ^ ".json")
        final_pr
    in
    gotfunc, cachefunc, donefunc

  end in
  let rec nextfunc () =
    let x =
      try
        Queue.pop q
      with
        | Queue.Empty -> raise Finished
    in
    if cachefunc x then nextfunc ()
    else x
  in
  let children =
    List.map
      (fun _ -> new pplacer_process partial gotfunc nextfunc progressfunc)
      (Base.range (Prefs.children prefs)) in
  event_loop children;
  donefunc ()

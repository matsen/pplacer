open Ppatteries
open Multiprocessing

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
          Some (Legacy.Marshal.from_channel rd)
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
        | None -> Legacy.close_in rd; Legacy.close_out wr
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
    | Data x -> List.iter gotfunc x; self#push
    | Exception exn
    | Fatal_exception exn -> raise (Child_error exn)

  method progress_received = progressfunc
end

let premask seq_type ref_align query_list =
  let base_map = match seq_type with
    | Alignment.Nucleotide_seq -> Nuc_models.nuc_map
    | Alignment.Protein_seq -> Prot_models.prot_map
  and n_sites = Alignment.length ref_align in
  let check_seq (name, seq) =
    String.iter
      (fun c ->
        if not (CharMap.mem c base_map) then
          failwith (Printf.sprintf "%c is not a known base in %s" c name))
      seq
  and initial_mask = Array.make n_sites false in
  (* Turn an alignment enum into a bool array mask which represents if any
   * site seen in the alignment was informative. *)
  let mask_of_enum enum =
    let mask = Array.copy initial_mask in
    Enum.iter
      (tap check_seq
       |- snd
       |- String.iteri
           (fun i c -> if Alignment.informative c then mask.(i) <- true))
      enum;
    mask
  in
  let ref_mask = Array.enum ref_align |> mask_of_enum in
  (* This function takes a sequence string and returns if it overlaps any
   * informative column of the reference sequence. *)
  let overlaps_mask s = String.enum s
    |> Enum.map Alignment.informative
    |> curry Enum.combine (Array.enum ref_mask)
    |> Enum.exists (uncurry (&&))
  in
  (try
     let seq, _ = List.find (snd |- overlaps_mask |- not) query_list in
     failwith (Printf.sprintf "Sequence %s doesn't overlap any reference sequences." seq)
   with Not_found -> ());
  (* Mask out sites that are either all gap in the reference alignment or
   * all gap in the query alignment. *)
  let mask = Array.map2
    (&&)
    ref_mask
    (List.enum query_list |> mask_of_enum)
  in
  let masklen = Array.fold_left
    (fun accum -> function true -> accum + 1 | _ -> accum)
    0
    mask
  in
  let cut_from_mask (name, seq) =
    let seq' = String.create masklen
    and pos = ref 0 in
    Array.iteri
      (fun e not_masked ->
        if not_masked then
          (seq'.[!pos] <- seq.[e];
           incr pos))
      mask;
    name, seq'
  in
  dprintf "sequence length cut from %d to %d.\n" n_sites masklen;
  let effective_length = match seq_type with
    | Alignment.Nucleotide_seq -> masklen
    | Alignment.Protein_seq -> 3*masklen
  in
  if effective_length = 0 then
    (print_endline
       "Sequence length cut to 0 by pre-masking; can't proceed with no information.";
     exit 1)
  else if effective_length <= 10 then
    dprintf
      "WARNING: you have %d sites after pre-masking. \
      That means there is very little information in these sequences for placement.\n"
      masklen
  else if effective_length <= 100 then
    dprintf
      "Note: you have %d sites after pre-masking. \
      That means there is rather little information in these sequences for placement.\n"
      masklen;
  let query_list' = List.map cut_from_mask query_list
  and ref_align' = Array.map cut_from_mask ref_align in
  query_list', ref_align', masklen, Some mask

let check_query n_sites query_list =
  begin match begin
    try
      Some
        (List.find
           (fun (_, seq) -> (String.length seq) != n_sites)
           query_list)
    with
      | Not_found -> None
  end with
    | Some (name, seq) ->
      Printf.printf
        "query %s is not the same length as the reference alignment (got %d; expected %d)\n"
        name
        (String.length seq)
        n_sites;
      exit 1;
    | None -> ()
  end


let run_placements prefs rp query_list from_input_alignment placerun_name placerun_cb =
  let timings = ref StringMap.empty in
  let orig_ref_tree = Refpkg.get_ref_tree rp in
  let ref_tree = Newick_gtree.add_zero_root_bl orig_ref_tree in
  let ref_align =
    try
      Refpkg.get_aln_fasta rp
    with Refpkg.Missing_element "aln_fasta" ->
      failwith
        "Please specify a reference alignment with -r or -c, or include all \
        reference sequences in the primary alignment."
  in
  if !verbosity >= 2 then begin
    print_endline "found in reference alignment: ";
    Ppr.print_string_array (Alignment.get_name_arr ref_align)
  end;

  let _ =
    try
      Refpkg.get_model rp
    with Refpkg.Missing_element "tree_stats" ->
      failwith "please specify a tree model with -s or -c"
  in
  if Newick_gtree.has_zero_bls ref_tree then
    dprint
      "WARNING: your tree has zero pendant branch lengths. \
      This can lead to zero likelihood values which will keep you from being \
      able to place sequences. You can remove identical sequences with \
      seqmagick.\n";

  let n_sites = Alignment.length ref_align in
  check_query n_sites query_list;

  let m, i = Refpkg.get_model rp in
  let module Model = (val m: Glvm.Model) in
  let module Glv = Model.Glv in
  let module Glv_arr = Glv_arr.Make(Model) in
  let module Like_stree = Like_stree.Make(Model) in
  let model = Model.build ref_align i in
  (* *** pre masking *** *)
  let query_list, ref_align, n_sites, mask =
    if Prefs.no_pre_mask prefs then
      query_list, ref_align, n_sites, None
    else begin
      dprint "Pre-masking sequences... ";
      premask (Model.seq_type model) ref_align query_list
    end
  in

  if Option.is_some mask && (Prefs.pre_masked_file prefs) <> "" then begin
    let ch = open_out (Prefs.pre_masked_file prefs) in
    let write_line = Alignment.write_fasta_line ch in
    Array.iter write_line ref_align;
    List.iter write_line query_list;
    close_out ch;
    exit 0;
  end;

  (* *** deduplicate sequences *** *)
  (* seq_tbl maps from sequence to the names that correspond to that seq. *)
  let seq_tbl = Hashtbl.create 1024 in
  List.iter
    (fun (name, seq) -> Hashtbl.replace
      seq_tbl
      seq
      (name ::
         try
           Hashtbl.find seq_tbl seq
         with
           | Not_found -> []))
    query_list;
  (* redup_tbl maps from the first entry of namel to the rest of the namel. *)
  let redup_tbl = Hashtbl.create 1024 in
  (* query_list is now deduped. *)
  let query_list = Hashtbl.fold
    (fun seq namel accum ->
      let hd = List.hd namel in
      List.iter
        (Hashtbl.add redup_tbl hd)
        (Pquery.uniform_namel namel);
      (hd, seq) :: accum)
    seq_tbl
    []
  in

  if !verbosity >= 1 &&
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
    dprint "everything looks OK.\n";
    exit 0;
  end;
  dprint "Determining figs... ";
  let figs = Fig.figs_of_gtree (Prefs.fig_cutoff prefs) ref_tree in
  begin match Prefs.fig_cutoff prefs with
    | 0. -> dprint "figs disabled.\n"
    | _ -> dprintf "%d figs.\n" (Fig.length figs);
      if Prefs.fig_tree prefs <> "" then
        Decor_gtree.of_newick_gtree orig_ref_tree
        |> flip Fig.onto_decor_gtree figs
        |> Phyloxml.gtree_to_file (Prefs.fig_tree prefs)
  end;
  let curr_time = Sys.time () in
  (* calculate like on ref tree *)
  dprint "Allocating memory for internal nodes... ";
  (* allocate our memory *)
  let darr = Like_stree.glv_arr_for model ref_tree n_sites in
  let parr = Glv_arr.mimic darr
  and snodes = Glv_arr.mimic darr
  in
  let util_glv = Glv.mimic (Glv_arr.get_one snodes) in
  dprint "done.\n";

  Option.may (Model.mask_sites model) mask;
  (* Refine the model if it's not coming directly from the reference package or
   * if the user wants it. *)
  if from_input_alignment || Prefs.always_refine prefs then
    Model.refine model n_sites ref_tree like_aln_map darr parr;
  Model.check model ref_align;
  if !verbosity >= 2 then Model.write stdout model;

  dprint "Caching likelihood information on reference tree... ";
  Like_stree.calc_distal_and_proximal model ref_tree like_aln_map
    util_glv ~distal_glv_arr:darr ~proximal_glv_arr:parr
    ~util_glv_arr:snodes;
  dprint "done.\n";

  timings := StringMap.add_listly
    "tree likelihood"
    ((Sys.time ()) -. curr_time)
    (!timings);
  (* pull exponents *)
  dprint "Pulling exponents... ";
  List.iter
    (Glv_arr.iter (Glv.perhaps_pull_exponent (-10)))
    [darr; parr;];
  dprint "done.\n";
  (* baseball calculation *)
  let half_bl_fun loc = (Gtree.get_bl ref_tree loc) /. 2. in
  dprint "Preparing the edges for baseball... ";
  Glv_arr.prep_supernodes model ~dst:snodes darr parr half_bl_fun;
  dprint "done.\n";

  (* *** check tree likelihood *** *)
  if Prefs.check_like prefs then begin
    let fp_check g str =
      let fpc = Glv.fp_classify g in
      if fpc > FP_zero then
        Printf.printf "%s is a %s\n" str (string_of_fpclass fpc)
    in
    let utilv_nsites = Gsl_vector.create n_sites
    and util_d = Glv.mimic darr.(0)
    and util_p = Glv.mimic parr.(0)
    and util_one = Glv.mimic darr.(0)
    in
    Glv.set_unit util_one;
    Array.mapi
      (fun i d ->
        let p = parr.(i)
        and sn = snodes.(i) in
        fp_check d (Printf.sprintf "distal %d" i);
        fp_check p (Printf.sprintf "proximal %d" i);
        fp_check sn (Printf.sprintf "supernode %d" i);
        Model.evolve_into model ~src:d ~dst:util_d (half_bl_fun i);
        Model.evolve_into model ~src:p ~dst:util_p (half_bl_fun i);
        Array.map (Printf.sprintf "%g")
          [| float_of_int i;
             Model.log_like3 model utilv_nsites util_d util_p util_one;
             Model.slow_log_like3 model util_d util_p util_one;
             Glv.logdot utilv_nsites sn util_one;
          |])
      darr
    |> Array.append
        [|[|"node"; "tree_likelihood"; "slow_tree_like"; "supernode_like"|]|]
    |> String_matrix.pad
    |> Array.iter (Array.iter (dprintf "%s  ") |- tap (fun () -> dprint "\n"))

  end;

  (* *** analyze query sequences *** *)
  let prior =
    if Prefs.uniform_prior prefs then Core.Uniform_prior
    else if Prefs.informative_prior prefs then
    (* Below: add on the prior lower bound onto the top branch length. *)
      Core.Informative_exp_prior
        (Core.prior_mean_map ((+.) (Prefs.prior_lower prefs)) ref_tree)
    else Core.Flat_exp_prior
      (* exponential with mean = average branch length *)
      ((Gtree.tree_length ref_tree) /.
        (float_of_int (Gtree.n_edges ref_tree)))
  in
  let partial = Core.pplacer_core
    (module Model: Glvm.Model with type t = Model.t and type glv_t = Model.glv_t)
    prefs figs prior model ref_align ref_tree ~darr ~parr ~snodes
  in
  let show_query = progress_displayer
    "working on %s (%d/%d)..."
    (List.length query_list)
  and n_done = ref 0 in
  let progressfunc msg =
    if String.rcontains_from msg 0 '>' then begin
      incr n_done;
      let query_name = String.sub msg 1 ((String.length msg) - 1) in
      show_query query_name
    end else
      dprintf "%s\n" msg
  in
  let q = Queue.create () in
  List.iter
    (fun (name, seq) -> Queue.push (name, (String.uppercase seq)) q)
    query_list;

  (* functions called respectively: when a result is received from a child
   * process, to determine if a sequence should be send to a child process, and
   * when all child processes have finished. *)
  let gotfunc, cachefunc, donefunc = if Prefs.fantasy prefs <> 0. then begin
    (* fantasy baseball *)
    let fantasy_mat =
      Fantasy.make_fantasy_matrix
        ~max_strike_box:(int_of_float (Prefs.strike_box prefs))
        ~max_strikes:(Prefs.max_strikes prefs)
    and fantasy_mod = round (100. *. (Prefs.fantasy_frac prefs))
    and n_fantasies = ref 0
    in
    let rec gotfunc = function
      | Core.Fantasy f ->
        Fantasy.add_to_fantasy_matrix f fantasy_mat;
        incr n_fantasies;
      | _ -> failwith "expected fantasy result"
    and cachefunc _ =
      (* if cachefunc returns true, the current thing is skipped; modulo
       * division will be 0 for every N things *)
      let res = (!n_done) mod fantasy_mod <> 0 in
      incr n_done;
      res
    and donefunc () =
      Fantasy.results_to_file placerun_name fantasy_mat (!n_fantasies);
      Fantasy.print_optimum fantasy_mat (Prefs.fantasy prefs) (!n_fantasies);
    in gotfunc, cachefunc, donefunc

  end else begin
    (* not fantasy baseball *)
    let map_fasta_file = Prefs.map_fasta prefs in
    let do_map = map_fasta_file <> "" || Prefs.map_identity prefs in
    (* similar to gotfunc/donefunc, but called respectively when a pquery is
     * received and after all pqueries have been received. *)
    let pquery_gotfunc, pquery_donefunc = if do_map then begin
      (* start: build the Maximum A Posteriori sequences *)
      let mrcam = Refpkg.get_mrcam rp
      and td = Refpkg.get_taxonomy rp
      and glvs = Glv_arr.make
        model
        ~n_glvs:2
        ~n_sites
      in
      let map_map = Map_seq.of_map
        (module Model: Glvm.Model with type t = Model.t and type glv_t = Model.glv_t)
        glvs.(0)
        glvs.(1)
        model
        ref_tree
        ~darr
        ~parr
        mrcam
        (Prefs.map_cutoff prefs)
      and result_map = ref IntMap.empty in
      let gotfunc pq =
        let best_placement = Pquery.best_place Placement.ml_ratio pq in
        result_map := IntMap.add_listly
          (Placement.location best_placement)
          ((List.hd (Pquery.namel pq)), (Pquery.seq pq))
          (!result_map);
        if not (Prefs.map_identity prefs) then pq else
          let placement_map = List.fold_left
            (fun accum p ->
              IntMap.add_listly
                (Placement.location p)
                p
                accum)
            IntMap.empty
            (Pquery.place_list pq)
          in
          let placement_map' = Map_seq.mrca_map_seq_map
            placement_map
            mrcam
            (ref_tree.Gtree.stree)
          in
          let identity = Alignment.identity (Pquery.seq pq) in
          let placements' = IntMap.fold
            (fun mrca pl accum ->
              List.fold_left
                (fun accum p ->
                  let map_seq = IntMap.find mrca map_map in
                  (Placement.add_map_identity p (identity map_seq)) :: accum)
                accum
                pl)
            placement_map'
            []
          in
          {pq with Pquery.place_list = placements'}
      and donefunc = if map_fasta_file = "" then identity else fun () ->
        let seq_map = Map_seq.mrca_map_seq_map
          (!result_map)
          mrcam
          (ref_tree.Gtree.stree)
        and space = Str.regexp " " in
        let map_fasta = IntMap.fold
          (fun i mrca accum ->
            if not (IntMap.mem i seq_map) then accum else
              let tax_name = match mrca with
                | Tax_id.NoTax -> "none"
                | _ -> Tax_taxonomy.get_tax_name td mrca
              in
              List.rev_append
                (IntMap.find i seq_map)
                (((Printf.sprintf "%d_%s"
                     i
                     (Str.global_replace space "_" tax_name)),
                  IntMap.find i map_map)
                 :: accum))
          mrcam
          []
        in
        Alignment.to_fasta
          (Array.of_list (List.rev map_fasta))
          map_fasta_file
      in
      gotfunc, donefunc
    (* end: build the Maximum A Posteriori sequences *)
    end else (identity, identity)
    in

    let classify =
      if Refpkg.tax_equipped rp then
        if Prefs.mrca_class prefs then
          Refpkg.mrca_classify rp
        else
         Tax_classify.classify_pr
           Placement.add_classif
           (Tax_classify.paint_classify (Edge_painting.of_refpkg rp))
      else
        identity
    and queries = ref [] in
    let rec gotfunc = function
      | Core.Pquery pq when not (Pquery.is_placed pq) ->
        dprintf "warning: %d identical sequences (including %s) were \
                 unplaced and omitted\n"
          (Pquery.namel pq |> List.length)
          (Pquery.name pq)
      | Core.Pquery pq ->
        let pq = pquery_gotfunc pq in
        queries := pq :: (!queries)
      | _ -> failwith "expected pquery result"
    and cachefunc _ = false
    and donefunc () =
      pquery_donefunc ();
      Placerun.make orig_ref_tree placerun_name (!queries)
        |> Placerun.redup redup_tbl
        |> classify
        |> placerun_cb
    in
    gotfunc, cachefunc, donefunc

  end in
  let gotfunc = function
    | Core.Timing (name, value) ->
      timings := StringMap.add_listly name value (!timings);
    | x -> gotfunc x
  in

  let gotfunc, donefunc =
    if Prefs.evaluate_all prefs then
      let evaluations = RefList.empty ()
      and total_logdots = ref 0 in
      (function
        | Core.Evaluated_best (seq, logdots, blc, bls) ->
          RefList.push evaluations (seq, blc, bls);
          total_logdots := logdots + !total_logdots
        | x -> gotfunc x),
      (fun () ->
        dprint ~l:2 "Sequences without their best location chosen:\n";
        dprintf "Evaluation: %g%% from %d logdots.\n"
          (* count the percentage of sequences where the best seen location was
           * also the complete best *)
          (if RefList.is_empty evaluations then 0.
           else
              RefList.fold_left
                (fun (count, total) (seq, blc, bls) ->
                  if blc <> bls then dprintf ~l:2 " - %s (%d vs. %d)\n" seq blc bls;
                  (if blc = bls then 1. +. count else count), total +. 0.01)
                (0., 0.)
                evaluations
              |> uncurry (/.))
          !total_logdots;
        if Prefs.evaluation_discrepancy prefs <> "" then begin
          let dt = Fig.onto_decor_gtree
            (Decor_gtree.of_newick_gtree orig_ref_tree)
            figs
          in
          RefList.to_list evaluations
          |> List.filter_map
              (* also decorate a fig tree with the best seen and complete best
               * locations if they're not the same *)
              (function
                | _, blc, bls when blc = bls -> None
                | seq, blc, bls ->
                  Gtree.get_bark_map dt
                  |> Decor_gtree.map_add_decor_listly blc
                      [Decor.Taxinfo (Tax_id.of_string "complete", "complete")]
                  |> Decor_gtree.map_add_decor_listly bls
                      [Decor.Taxinfo (Tax_id.of_string "seen", "seen")]
                  |> Gtree.set_bark_map dt
                  |> (fun t -> Some (Some seq, t)))
          |> Phyloxml.named_gtrees_to_file (Prefs.evaluation_discrepancy prefs)
        end;
        donefunc ())
    else
      gotfunc, donefunc
  in

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
  begin match
    Multiprocessing.try_fork
      (Prefs.children prefs)
      (new pplacer_process partial gotfunc nextfunc)
      progressfunc
  with
  | [] ->
    dprint "couldn't fork any children; falling back to a single process.\n";
    let rec aux () =
      match begin
        try
          Some (nextfunc ())
        with Finished -> None
      end with
      | None -> ()
      | Some query ->
        partial ~show_query query |> List.iter gotfunc; aux ()
    in
    aux ()
  | children -> event_loop children
  end;
  donefunc ();
  if Prefs.timing prefs then begin
    Printf.printf "\ntiming data:\n";
    StringMap.iter
      (fun name values ->
        Printf.printf "  %s: %0.4fs\n" name (List.fsum values))
      (!timings)
  end

let partition_queries ref_name_set aln =
  let ref_aln, query_aln =
    Array.partition
      (fun (name,_) -> StringSet.mem name ref_name_set)
      aln
  in
  query_aln,
  if Array.length ref_aln = 0 then begin
    dprint
      "Didn't find any reference sequences in given alignment file. \
         Using supplied reference alignment.\n";
    None
  end
  else begin
    dprint
      "Found reference sequences in given alignment file. \
         Using those for reference alignment.\n";
    Some ref_aln
  end

let run_file prefs query_fname =
  dprintf
    "Running pplacer %s analysis on %s...\n"
    Version.version
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
  let file_path = Refpkg_parse.file_path in
  let rp_strmap =
    List.fold_right
    (* only set if the option string is non empty.
     * override the contents of the reference package. *)
      (fun (k,v) m ->
        if v = "" then m
        else StringMap.add k (ref_dir_complete^v |> file_path) m)
      [
        "tree", Prefs.tree_fname prefs;
        "aln_fasta", Prefs.ref_align_fname prefs;
        "tree_stats", Prefs.stats_fname prefs;
      ]
      (match Prefs.refpkg_path prefs with
        | "" ->
            StringMap.add "name"
              (Prefs.ref_align_fname prefs
               |> safe_chop_extension
               |> Refpkg_parse.metadata)
              StringMap.empty
        | path -> Refpkg_parse.strmap_of_path path)
  in
  if not (StringMap.mem "tree" rp_strmap) then
    if Prefs.refpkg_path prefs = "" then
      failwith "please specify a reference tree with -t or -c"
    else
      failwith "the reference package provided does not contain a tree";

  let rp = Refpkg.of_strmap
    ~ignore_version:(Prefs.refpkg_path prefs = "")
    prefs
    rp_strmap
  in
  let ref_tree = Refpkg.get_ref_tree rp in
  (* *** split the sequences into a ref_aln and a query_list *** *)
  let ref_name_map = Newick_gtree.leaf_label_map ref_tree in
  let ref_name_set = IntMap.values ref_name_map |> StringSet.of_enum in
  if IntMap.cardinal ref_name_map <> StringSet.cardinal ref_name_set then
    failwith("Repeated names in reference tree!");
  let query_align, ref_align = Alignment.upper_aln_of_any_file query_fname
    |> partition_queries ref_name_set
  in
  let _ = Array.fold_left
    (fun seen (seq, _) ->
      if StringSet.mem seq seen then
        failwith
          (Printf.sprintf
             "duplicate reference sequence '%s' in query file %s"
             seq
             query_fname);
      StringSet.add seq seen)
    StringSet.empty
    (Option.default [||] ref_align)
  in ();
  let rp = Option.map_default (flip Refpkg.set_aln_fasta rp) rp ref_align
  and query_bname = Filename.basename (Filename.chop_extension query_fname)
  and from_input_alignment = Option.is_some ref_align in
  let jplace_name = match Prefs.out_file prefs with
    | "" -> ((Prefs.out_dir prefs) ^ "/" ^ query_bname ^ ".jplace")
    | x -> x in
  let placerun_cb pr =
    Placerun_io.to_json_file
      jplace_name
      pr
  and query_list = Array.to_list query_align
  and n_groups = Prefs.groups prefs in
  if n_groups > 1 then begin
    dprintf "Splitting query alignment into %d groups... " n_groups;
    let groups = Seq_group.group n_groups query_list in
    let prl = RefList.empty () in
    dprint "done.\n";
    Array.iteri
      (fun i ql ->
        dprintf "Running sequence group %d of %d.\n" (succ i) n_groups;
        if not (List.is_empty ql) then
          run_placements
            prefs rp ql from_input_alignment query_bname (RefList.push prl))
      groups;
    RefList.to_list prl
      |> List.reduce (Placerun.combine query_bname)
      |> placerun_cb
  end else begin
    run_placements
      prefs rp query_list from_input_alignment query_bname placerun_cb
  end


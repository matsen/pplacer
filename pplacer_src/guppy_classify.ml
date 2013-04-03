open Subcommand
open Guppy_cmdobjs
open Ppatteries

(* TIAMR = Taxid Algebraic Map Real. These will store the confidence that we
have in various taxonomic classifications. *)
module TIAMR = AlgMapR (Tax_id.TaxIdMap)

(* The lower case tiamr also associates a place_id with this classification. *)
type tiamr = {
  tiamr: float TIAMR.t;
  place_id: int64;
}

(* A classification at every rank and perhaps a Bayes_factor *)
type classification = {
 (* tiamrim = tiamr IntMap; the integer key is the rank.
  Rank increases from zero, with zero being the "root". *)
  tiamrim: tiamr IntMap.t;
  bayes_factors: Bayes_factor.t option;
}

type seq_classifications = classification StringMap.t

let classification ?bayes_factors place_id tiamrim =
  let tiamrim = IntMap.map (fun tiamr -> {tiamr; place_id}) tiamrim in
  {tiamrim; bayes_factors}
let subclassification ?bayes_factors tiamrim = {tiamrim; bayes_factors}

let tiamr {tiamr} = tiamr
let set_tiamr t tiamr = {t with tiamr}
let map_tiamr t f = {t with tiamr = f t.tiamr}

let tiamrim {tiamrim} = tiamrim
let bayes_factors {bayes_factors} = Option.get bayes_factors
let set_tiamrim cf tiamrim = {cf with tiamrim}
let map_tiamrim cf f = {cf with tiamrim = f cf.tiamrim}

(* If rank is less than the tax rank of ti, then move up the taxonomy until
 * the first time that the tax rank is less than or equal to rank. *)
let classify_at_rank td rank ti =
  let rec aux curr_ti =
    if rank >= Tax_taxonomy.get_tax_rank td curr_ti then
      curr_ti
    else
      Tax_taxonomy.get_ancestor td curr_ti |> aux
  in
  aux ti

(* Apply f to all of the keys and add the results together. *)
let keymap_add_by f m =
  List.fold_right
    (fun (k,v) -> (TIAMR.add_by (f k) v))
    (TIAMR.to_pairs m)
    TIAMR.empty

(* This is the equivalent to filtering by `rank = desired_rank` in the
 * old best_classifications view. *)
let tiamr_at_rank td rank tiamr =
  TIAMR.filteri (fun ti _ -> Tax_taxonomy.get_tax_rank td ti = rank) tiamr
  |> junction TIAMR.is_empty (const None) some

(* From a map from the edges to the confidence we have in their placements,
produce a full classification map. *)
let partition_by_rank td tiamr =
  let outmap = ref IntMap.empty
  and m = ref tiamr
  and n_ranks = Tax_taxonomy.get_n_ranks td in
  for desired_rank=(n_ranks-1) downto 0 do
    m := keymap_add_by (classify_at_rank td desired_rank) !m;
    outmap := IntMap.opt_add
      desired_rank
      (tiamr_at_rank td desired_rank !m)
      !outmap
  done;
  !outmap

(* Produce a classification map for each pquery in a placerun using pplacer
 * classification. The callback function `f` will be called for each pquery. *)
let placerun_classify how criterion td f pr =
  let prn = Placerun.get_name pr in
  List.iter
    (fun pq ->
      partition_by_rank
        td
        (List.fold_right
           (fun p -> TIAMR.add_by (how p) (criterion p))
           (Pquery.place_list pq)
           (TIAMR.empty))
      |> f prn pq)
    (Placerun.get_pqueries pr)

(* From a classificication map, find the best classifications for each rank. *)
let filter_best ?multiclass_min cutoff cf =
  IntMap.filter_map (fun _ {tiamr; place_id} ->
    match TIAMR.enum tiamr |> Enum.arg_max snd, multiclass_min with
    (* If there's a clear best, pick that. *)
    | (t, l), _ when l >= cutoff ->
      Some {tiamr = TIAMR.singleton t l; place_id}
    | _, None -> None
    | _, Some multiclass_min ->
      (* Otherwise, if we're multiclassifying, see if it adds up. *)
      (* Filter out the ones that are less than multiclass_min *)
      TIAMR.filter (fun l -> l >= multiclass_min) tiamr
      (* `let junction pred f g a = if pred a then f a else g a`
       * So if we are in total less than the cutoff, then we keep the tiamr. *)
      |> junction
          (TIAMR.values |- Enum.fold (+.) 0. |- (fun s -> s >= cutoff))
          (fun tiamr -> Some {tiamr; place_id})
          (const None))
  |> map_tiamrim cf

let filter_best_by_bayes ?multiclass_min bayes_cutoff cf =
  let factors = bayes_factors cf in
  IntMap.backwards cf.tiamrim
  (* enum with keys in decreasing order, so here starting with lowest (away from
   * root, but highest number) rank. *)
  |> Enum.fold
      (fun (found_evidence, accum) (rank, value) ->
        (* If we have found evidence for a rank below, continue to accumulate
         * all taxonomic classifications in ranks above. *)
        if found_evidence then true, IntMap.add rank value accum else (* ... *)
        match factors.(rank) with
        | _, _, Some evidence when evidence >= bayes_cutoff ->
          true, IntMap.add rank value accum
        | _ -> false, accum)
      (false, IntMap.empty)
  |> snd (* Get accum, the map of ranks to tiamr's. *)
  |> (match multiclass_min with
    | None -> identity
    | Some multiclass_min ->
      IntMap.filter_map
        (fun _ t ->
          TIAMR.filter (fun l -> l >= multiclass_min) t.tiamr
          |> junction TIAMR.is_empty (const None) (set_tiamr t |- some)))
  |> set_tiamrim cf

(* For every rank, find the first rank at or above it with a valid set of
 * classifications. *)
let find_ranks_per_want_rank td cf =
  let rec aux rank =
    match IntMap.Exceptionless.find rank cf.tiamrim with
    | Some tiamr -> tiamr
    | _ when rank = 0 -> {tiamr = TIAMR.empty; place_id = -1L}
    | _ -> aux (rank - 1)
  in
  0 --^ Tax_taxonomy.get_n_ranks td
  |> Enum.map (identity &&& aux)
  |> IntMap.of_enum

(* let maybe_cl = function *)
(*   | Some (_, cl) as x when not (List.is_empty cl) -> x *)
(*   | _ -> None *)

(* Is parent anywhere on the lineage of child? *)
let on_lineage td parent child =
  Tax_taxonomy.get_lineage td child |> List.mem parent

let filter_ranks_below rank tiamrim =
  IntMap.split (succ rank) tiamrim |> Tuple3.first

let best_classification {tiamr} =
  TIAMR.enum tiamr |> Enum.arg_max snd |> fst

let mrca td {tiamr} =
  TIAMR.keys tiamr |> List.of_enum |> Tax_taxonomy.list_mrca td

let merge_fn f _ a b =
  match a, b with
  | None, None -> None
  | Some x, None
  | None, Some x -> Some x
  | Some a, Some b -> Some (f a b)

(* Get the maximum binding in a.tiamrim or return lbl b. When applied to a
tiamrim this gives the most specific classification that the tiamrim has to
offer. If a is empty just return lbl b. *)
let max_tiamrim_or_return_other lbl a b =
  try
    IntMap.max_binding a.tiamrim
  with Invalid_argument _ -> Return.return lbl b

let label3 f a b c =
  Return.label (f a b c)

(* UI-related *)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit sqlite_cmd () as super_sqlite
  inherit rng_cmd () as super_rng

  val classifier = flag "--classifier"
    (Formatted ("pplacer", "Which classifier to use, out of 'pplacer', 'nbc', 'hybrid2', 'hybrid5' or 'rdp'. default: %s"))
  val cutoff = flag "--cutoff"
    (Formatted (0.9, "The default value for the likelihood_cutoff param. Default: %0.2f"))
  val bayes_cutoff = flag "--bayes-cutoff"
    (Formatted (1., "The default value for the bayes_cutoff param. Default: %0.2f"))
  val multiclass_min = flag "--multiclass-min"
    (Formatted (0.2, "The default value for the multiclass_min param. Default: %0.2f"))
  val bootstrap_cutoff = flag "--bootstrap-cutoff"
    (Formatted (0.8, "The default value for the bootstrap_cutoff param. Default: %0.2f"))
  val bootstrap_extension_cutoff = flag "--bootstrap-extension-cutoff"
    (Formatted (0.4, "The default value for the bootstrap_cutoff param. Default: %0.2f"))

  val use_pp = flag "--pp"
    (Plain (false, "Use posterior probability for our criteria in the pplacer classifier."))
  val tax_identity = flag "--tax-median-identity-from"
    (Needs_argument ("", "Calculate the median identity for each sequence per-tax_id from the specified alignment."))
  val mrca_class = flag "--mrca-class"
    (Plain (false, "Classify against a placefile that was generated with MRCA classification"))

  val nbc_sequences = flag "--nbc-sequences"
    (Needs_argument ("nbc query sequences", "The query sequences to use for the NBC classifier. \
                                             Can be specified multiple times for multiple inputs."))
  val word_length = flag "--word-length"
    (Formatted (8, "The length of the words used for NBC classification. default: %d"))
  val nbc_rank = flag "--nbc-rank"
    (Formatted ("genus", "The desired most specific rank for NBC classification. \
                            'all' puts each sequence in the classifier at every rank of its lineage. default: %s"))
  val n_boot = flag "--n-boot"
    (Formatted (100, "The number of times to bootstrap a sequence with the NBC classifier. 0 = no bootstrap. default: %d"))
  val children = flag "-j"
    (Formatted (2, "The number of processes to spawn to do NBC classification. default: %d"))
  val no_pre_mask = flag "--no-pre-mask"
    (Plain (false, "Don't pre-mask the sequences for NBC classification."))
  val nbc_counts = flag "--nbc-counts"
    (Needs_argument ("", "Read/write NBC k-mer counts to the given file. File cannot be NFS mounted."))
  val nbc_as_rdp = flag "--nbc-as-rdp"
    (Plain (false, "Do NBC classification like RDP: find the lineage of the full-sequence classification, \
                    then bootstrap to find support for it."))

  val rdp_results = flag "--rdp-results"
    (Needs_argument ("rdp results", "The RDP results file for use with the RDP classifier. \
                                     Can be specified multiple times for multiple inputs."))

  val blast_results = flag "--blast-results"
    (Needs_argument ("BLAST results", "The BLAST results file for use with the BLAST classifier. \
                                       Can be specified multiple times for multiple inputs."))
  val random_tie_break = flag "--no-random-tie-break"
    (Plain (true, "Break NBC ties randomly."))

  method specl =
    super_refpkg#specl
  @ super_sqlite#specl
  @ super_rng#specl
  @ [
    string_flag classifier;
    float_flag cutoff;
    float_flag bayes_cutoff;
    float_flag multiclass_min;
    float_flag bootstrap_cutoff;
    float_flag bootstrap_extension_cutoff;
    toggle_flag use_pp;
    string_flag tax_identity;
    toggle_flag mrca_class;
    delimited_list_flag nbc_sequences;
    int_flag word_length;
    string_flag nbc_rank;
    int_flag n_boot;
    int_flag children;
    toggle_flag no_pre_mask;
    string_flag nbc_counts;
    toggle_flag nbc_as_rdp;
    delimited_list_flag rdp_results;
    delimited_list_flag blast_results;
    toggle_flag random_tie_break;
 ]

  method desc =
    "outputs classification information in SQLite format"
  method usage = "usage: classify [options] placefile[s]"

  method private merge_hybrid2 td pp nbc lbl =
    let pp_rank, pp_best = max_tiamrim_or_return_other lbl pp nbc
    and nbc_rank, nbc_best = max_tiamrim_or_return_other lbl nbc pp in
    if pp_rank >= nbc_rank
      (* on_lineage td parent child *)
      && on_lineage
        td
        (best_classification nbc_best)
        (best_classification pp_best)
    then pp
    else nbc

  method private merge_fake_hybrid2 td pp nbc lbl =
    let pp_rank, pp_best = max_tiamrim_or_return_other lbl pp nbc
    and nbc_rank, nbc_best = max_tiamrim_or_return_other lbl nbc pp in
    if false && pp_rank >= nbc_rank
      (* on_lineage td parent child *)
      && on_lineage
        td
        (best_classification nbc_best)
        (best_classification pp_best)
    then pp
    else nbc

  method private merge_hybrid5 td pp nbc lbl =
    let nbc_rank, nbc_best = max_tiamrim_or_return_other lbl nbc pp in
    match IntMap.split nbc_rank pp.tiamrim with
    | _, Some pp_best, pp_above ->
      if IntMap.cardinal pp_above <= 1
        && on_lineage
          td
          (best_classification nbc_best)
          (best_classification pp_best)
      then pp
      else nbc
    | _ -> nbc

  method private nbc_classifier rp rank_idx infile =
    let query_aln = Alignment.upper_aln_of_any_file infile
    and n_boot = fv n_boot
    and word_length = fv word_length
    and rng = self#random_state in
    match fvo nbc_counts with
    | Some counts ->
      let map_file =
        if Sys.file_exists counts then
          Unix.openfile counts [Unix.O_RDONLY] 0o666, false
        else
          Unix.openfile counts [Unix.O_RDWR; Unix.O_CREAT] 0o666, true
      in
      Nbc.Classifier.of_refpkg ~n_boot ~map_file ~rng word_length rank_idx rp,
      Array.to_list query_aln
    | None ->
      let ref_aln = Refpkg.get_aln_fasta rp in
      let query_list, ref_aln, _, _ =
        if fv no_pre_mask then
          Array.to_list query_aln, ref_aln, 0, None
        else begin
          let ref_name_set = Array.enum ref_aln
            |> Enum.map fst
            |> StringSet.of_enum
          in
          let query_aln', ref_aln' =
            Pplacer_run.partition_queries ref_name_set query_aln
              |> second (Option.default ref_aln)
          in
          let n_sites = Alignment.length ref_aln'
          and query_list = Array.to_list query_aln' in
          Pplacer_run.check_query n_sites query_list;
          dprint "pre-masking sequences... ";
          Pplacer_run.premask Alignment.Nucleotide_seq ref_aln' query_list
        end
      in
      Nbc.Classifier.of_refpkg ~ref_aln ~n_boot ~rng word_length rank_idx rp,
      query_list

  method private placefile_action prl =
    self#set_default_seed;
    let rp = self#get_rp in
    let criterion = if (fv use_pp) then Placement.post_prob else Placement.ml_ratio in
    let td = Refpkg.get_taxonomy rp in

    let db = self#get_db in
    Sql.check_exec
      db
      ~cb:(fun row _ -> match row with
        | [| Some "1" |] -> ()
        | _ -> failwith "run `rppr prep_db` before running `guppy classify`")
      "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE name = 'placement_classifications')";
    Sql.check_exec db "BEGIN TRANSACTION";

    let st = Sqlite3.prepare db
      "INSERT INTO runs VALUES (NULL, ?)"
    in
    Sql.bind_step_reset db st
      [|Sql.D.TEXT (Array.to_list Sys.argv |> String.concat " ")|];
    let run_id = Sqlite3.last_insert_rowid db in
    let new_place_id =
      let p_st = Sqlite3.prepare db
        "INSERT INTO placements VALUES (NULL, ?, ?)"
      in
      fun classifier ->
        Sql.bind_step_reset db p_st
          [|Sql.D.TEXT classifier; Sql.D.INT run_id|];
        Sqlite3.last_insert_rowid db
    in

    let rec default_filter_nbc m = filter_best (fv bootstrap_cutoff) m
    and perform_one_nbc infile =
      let nbc_rank = fv nbc_rank
      and children = fv children in
      let rank_idx = match nbc_rank with
        | "auto" -> Nbc.Classifier.Auto_rank
        | "all" -> Nbc.Classifier.All_ranks
        | _ ->
          try
            Nbc.Classifier.Rank (Tax_taxonomy.get_rank_index td nbc_rank)
          with Not_found ->
            failwith (Printf.sprintf "invalid rank %s" nbc_rank)
      in
      let classif, query_list = self#nbc_classifier rp rank_idx infile in
      let rdp_filter =
        if fv nbc_as_rdp then fun seq ->
          let on_lineage =
            Nbc.Classifier.classify classif ~like_rdp:true
                                    ~random_tie_break:(fv random_tie_break) seq
            |> Tax_taxonomy.get_lineage td
            |> flip List.mem
          in
          TIAMR.filteri (fun tid _ -> on_lineage tid)
            |- junction TIAMR.is_empty (const None) some
            |> const
            |> IntMap.filter_map
        else const identity
      in
      let bootstrap seq =
        Alignment.ungap seq
          |> Nbc.Classifier.bootstrap
                            ~random_tie_break:(fv random_tie_break) classif
          |> partition_by_rank td
          |> rdp_filter seq
      and pn_st = Sqlite3.prepare db
        "INSERT INTO placement_names VALUES (?, ?, ?, 1);"
      and pc_st = Sqlite3.prepare db
        "INSERT INTO placement_nbc VALUES (?, ?, ?)"
      in

      let classify name boot_map accum =
        let place_id = new_place_id "nbc" in
        Sql.bind_step_reset db pn_st [|
          Sql.D.INT place_id;
          Sql.D.TEXT name;
          Sql.D.TEXT infile;
        |];
        flip IntMap.iter boot_map (fun _ likelihoods ->
          flip TIAMR.iter likelihoods (fun ti likelihood ->
            Sql.bind_step_reset db pc_st [|
              Sql.D.INT place_id;
              Sql.D.TEXT (Tax_id.to_string ti);
              Sql.D.FLOAT likelihood
        |]));
        StringMap.add
          name
          (classification place_id boot_map)
          accum
      and progress_handler = progress_displayer
        "classifying %s (%d/%d)..."
        (List.length query_list)
      in

      Multiprocessing.map
        ~children
        ~progress_handler
        (tap (fst |- print_endline) |- second bootstrap)
        query_list
      |> List.fold_left (uncurry classify |> flip) StringMap.empty
    and perform_nbc () =
      fv nbc_sequences
        |> List.map perform_one_nbc
        |> List.fold_left StringMap.union StringMap.empty

    and default_filter_pplacer =
      let multiclass_min = match fv multiclass_min with
        | v when v < 0.0 -> None
        | v -> Some v
      and bayes_cutoff = fv bayes_cutoff
      and cutoff = fv cutoff in
      fun m ->
        if bayes_cutoff =~ 0. then
          filter_best ?multiclass_min cutoff m
        else
          filter_best_by_bayes ?multiclass_min bayes_cutoff m
    and perform_pplacer () =
      let pn_st = Sqlite3.prepare db
        "INSERT INTO placement_names VALUES (?, ?, ?, ?);"
      and pc_st = Sqlite3.prepare db
        "INSERT INTO placement_classifications VALUES (?, ?, ?)"
      and pp_st = Sqlite3.prepare db
        "INSERT INTO placement_positions VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      and pe_st = Sqlite3.prepare db
        "INSERT INTO placement_evidence VALUES (?, ?, ?, ?)"
      and classif_map = Classif_map.of_refpkg_mrca_class rp (fv mrca_class)
      and best_classif_map = ref StringMap.empty in
      let lax_classif =
        let root_taxon = Classif_map.root_taxon classif_map in
        Placement.classif_opt |- Option.default root_taxon
      and bayes_factors =
        Bayes_factor.of_refpkg_and_classif_map rp classif_map criterion
      in

      let tax_identity_func = match fvo tax_identity with
        | None -> fun _ _ -> ()
        | Some aln_file ->
          let open Tax_id in
          let aln_map = Alignment.upper_aln_of_any_file aln_file
            |> Array.enum
            |> StringMap.of_enum
          and seqinfom = Refpkg.get_seqinfom rp in
          let refs_by_tax_id = Refpkg.get_ref_tree rp
            |> Newick_gtree.leaf_label_map
            |> IntMap.values
            |> Enum.map
                (Tax_seqinfo.tax_id_by_node_label seqinfom
                 &&& flip StringMap.find aln_map)
            |> List.of_enum
            |> TaxIdMap.of_pairlist_listly
          and pmi_st = Sqlite3.prepare db
            "INSERT INTO placement_median_identities VALUES (?, ?, ?)"
          in
          fun place_id pq ->
            let id = StringMap.find (Pquery.name pq) aln_map
              |> Alignment.identity
              |- fst
            in
            TaxIdMap.iter
              (fun ti seql ->
                Sql.bind_step_reset db pmi_st [|
                  Sql.D.INT place_id;
                  to_sql ti;
                  Sql.D.FLOAT
                    (List.map id seql |> List.sort compare |> median);
              |])
              refs_by_tax_id;
      in

      let classify prn pq rank_map =
        let place_id = new_place_id "pplacer" in
        List.iter
          (fun (name, mass) -> Sql.bind_step_reset db pn_st [|
            Sql.D.INT place_id;
            Sql.D.TEXT name;
            Sql.D.TEXT prn;
            Sql.D.FLOAT mass;
          |])
          (Pquery.namlom pq);
        flip IntMap.iter rank_map (fun _ tiamr ->
          flip TIAMR.iter tiamr (fun tax_id prob ->
            Sql.bind_step_reset db pc_st [|
              Sql.D.INT place_id;
              Tax_id.to_sql tax_id;
              Sql.D.FLOAT prob;
            |]));
        List.iter
          (fun p -> Sql.bind_step_reset db pp_st [|
            Sql.D.INT place_id;
            Sql.D.INT (Int64.of_int (Placement.location p));
            Sql.D.FLOAT (Placement.ml_ratio p);
            Sql.D.FLOAT (Placement.log_like p);
            Sql.D.FLOAT (Placement.distal_bl p);
            Sql.D.FLOAT (Placement.pendant_bl p);
            lax_classif p |> Tax_id.to_sql;
            (match Placement.map_identity_opt p with
              | None -> Sql.D.NULL
              | Some (ratio, _) -> Sql.D.FLOAT ratio);
            (match Placement.map_identity_opt p with
              | None -> Sql.D.NULL
              | Some (_, denom) -> Sql.D.INT (Int64.of_int denom));
          |])
          (Pquery.place_list pq);

        let bayes_factors = bayes_factors pq in
        Array.iter
          (fun (rank, ev, bf) -> Sql.bind_step_reset db pe_st [|
            Sql.D.INT place_id;
            Sql.D.TEXT rank;
            Sql.D.FLOAT ev;
            (match bf with
              | None -> Sql.D.NULL
              | Some bf -> Sql.D.FLOAT bf);
          |])
          bayes_factors;
        let bc = classification ~bayes_factors place_id rank_map in
        List.fold_left
          (flip StringMap.add bc |> flip)
          !best_classif_map
          (Pquery.namel pq)
        |> (:=) best_classif_map;
        tax_identity_func place_id pq;
      in
      List.iter (placerun_classify lax_classif criterion td classify) prl;
      !best_classif_map

    and default_filter_rdp m = filter_best (fv bootstrap_cutoff) m
    and perform_rdp () =
      let name_map = Tax_id.TaxIdMap.keys td.Tax_taxonomy.tax_name_map
        |> Enum.map (Guppy_to_rdp.get_mothur_name td &&& identity)
        |> StringMap.of_enum
      and pn_st = Sqlite3.prepare db
        "INSERT INTO placement_names VALUES (?, ?, ?, 1.);"
      and pc_st = Sqlite3.prepare db
        "INSERT INTO placement_nbc VALUES (?, ?, ?)"
      and best_classif_map = ref StringMap.empty
      and split_line = Str.regexp "[\t;]" |> Str.split in

      let process origin name rows =
        let place_id = new_place_id "rdp" in
        Sql.bind_step_reset db pn_st
          [|
            Sql.D.INT place_id;
            Sql.D.TEXT name;
            Sql.D.TEXT origin;
          |];
        List.iter
          (fun (tid, boot) ->
            Sql.bind_step_reset db pc_st
              [|
                Sql.D.INT place_id;
                Sql.D.TEXT (Tax_id.to_string tid);
                Sql.D.FLOAT boot;
              |])
          rows;

        List.enum rows
        |> Enum.map
            (fun (ti, boot) ->
              Tax_taxonomy.get_tax_rank td ti, TIAMR.singleton ti boot)
        |> IntMap.of_enum
        |> classification place_id
        |> flip (StringMap.add name) !best_classif_map
        |> (:=) best_classif_map

      and classify line =
        match split_line line with
        | name :: taxonomy ->
          name,
          List.map
            (fun s ->
              let tax_name, boot = Scanf.sscanf s "%s@(%g)" (curry identity) in
              (try
                 StringMap.find tax_name name_map
               with Not_found ->
                 failwith (tax_name ^ " not found in refpkg's taxonomy")),
              boot /. 100.)
            taxonomy
        | _ -> failwith (Printf.sprintf "malformed line: %S" line)
      in
      fv rdp_results
        |> List.enum
        |> Enum.map (identity &&& (File.lines_of |- Enum.map classify))
        |> Enum.iter (fun (a, bcl) -> Enum.iter (process a |> uncurry) bcl);

      !best_classif_map

    and perform_blast () =
      let ref_name_map = Refpkg.get_seqinfom rp
        |> StringMap.map (fun {Tax_seqinfo.tax_id} -> tax_id)
      and pn_st = Sqlite3.prepare db
        "INSERT INTO placement_names VALUES (?, ?, ?, 1.);"
      and pc_st = Sqlite3.prepare db
        "INSERT INTO placement_nbc VALUES (?, ?, ?)"
      and best_classif_map = ref StringMap.empty in

      let process origin (name, ref_name, pid) =
        let place_id = new_place_id "blast"
        and tax_id = StringMap.find ref_name ref_name_map in
        let tiamrim = TIAMR.singleton tax_id pid
          |> partition_by_rank td
        in

        Sql.bind_step_reset db pn_st
          [|
            Sql.D.INT place_id;
            Sql.D.TEXT name;
            Sql.D.TEXT origin;
          |];
        flip IntMap.iter tiamrim (fun _ tiamr ->
          flip TIAMR.iter tiamr (fun tid pid ->
            Sql.bind_step_reset db pc_st
              [|
                Sql.D.INT place_id;
                Sql.D.TEXT (Tax_id.to_string tid);
                Sql.D.FLOAT pid;
              |]));

        tiamrim
          |> classification place_id
          |> flip (StringMap.add name) !best_classif_map
          |> (:=) best_classif_map

      and classify line =
        let splut = String.nsplit line "\t" |> Array.of_list in
        splut.(0), splut.(1), float_of_string splut.(2) /. 100.

      in
      fv blast_results
        |> List.enum
        |> Enum.map
            (identity &&&
               (File.lines_of
                |- Enum.map classify
                |- Enum.group Tuple3.first
                |- Enum.map (Enum.arg_max Tuple3.third)))
        |> Enum.iter (fun (a, bcl) -> Enum.iter (process a) bcl);

      !best_classif_map

    in

    let default_pplacer () =
      perform_pplacer () |> StringMap.map default_filter_pplacer
    and default_nbc () = perform_nbc () |> StringMap.map default_filter_nbc
    and default_rdp () = perform_rdp () |> StringMap.map default_filter_rdp in
    let multiclass = match fv classifier with
      | "pplacer" -> default_pplacer ()
      | "nbc" -> default_nbc ()
      | "rdp" -> default_rdp ()
      | "blast" -> perform_blast ()
      | "hybrid2" ->
        StringMap.merge
          (label3 self#merge_hybrid2 td |> merge_fn)
          (default_pplacer ())
          (default_nbc ())
      | "hybrid2f" ->
        StringMap.merge
          (label3 self#merge_fake_hybrid2 td |> merge_fn)
          (default_pplacer ())
          (default_nbc ())
      | "hybrid5" ->
        StringMap.merge
          (label3 self#merge_hybrid5 td |> merge_fn)
          (default_pplacer ())
          (default_nbc ())
      | s -> failwith (Printf.sprintf "invalid classifier: %s" s)
    and mc_st = Sqlite3.prepare db
      "INSERT INTO multiclass VALUES (?, ?, ?, ?, ?, ?)"
    in
    flip StringMap.iter multiclass (fun seq_name cf ->
      flip IntMap.iter (find_ranks_per_want_rank td cf) (fun want_rank t ->
        flip TIAMR.iter t.tiamr (fun ti l -> Sql.bind_step_reset db mc_st [|
          Sql.D.INT t.place_id;
          Sql.D.TEXT seq_name;
          Sql.D.TEXT (Tax_taxonomy.get_rank_name td want_rank);
          Sql.D.TEXT (Tax_taxonomy.rank_name_of_tax_id td ti);
          Sql.D.TEXT (Tax_id.to_string ti);
          Sql.D.FLOAT l;
        |])));

    Sql.check_exec db "COMMIT";
    Sql.close db

end

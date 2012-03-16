open Subcommand
open Guppy_cmdobjs
open Ppatteries

module TIAMR = AlgMapR (Tax_id.TaxIdMap)

(* if rank is less than the tax rank of ti, then move up the taxonomy until
 * the first time that the tax rank is less than or equal to rank *)
let classify_at_rank td rank ti =
  let rec aux curr_ti =
    if rank >= Tax_taxonomy.get_tax_rank td curr_ti then
      curr_ti
    else
      Tax_taxonomy.get_ancestor td curr_ti |> aux
  in
  aux ti

(* apply f to all of the keys and add the results together *)
let keymap_add_by f m =
  List.fold_right
    (fun (k,v) -> (TIAMR.add_by (f k) v))
    (TIAMR.to_pairs m)
    TIAMR.empty

(* Produce a classification map for each pquery in a placerun using pplacer
 * classification. The callback function `f` will be called for each pquery. *)
let pplacer_classify how criterion td f pr =
  let n_ranks = Tax_taxonomy.get_n_ranks td
  and prn = Placerun.get_name pr in
  try
    List.iter
      (fun pq ->
        let outmap = ref IntMap.empty in
        let m = ref
          (List.fold_right
             (fun p -> TIAMR.add_by (how p) (criterion p))
             (Pquery.place_list pq)
             (TIAMR.empty))
        in
        for desired_rank=(n_ranks-1) downto 0 do
          m := keymap_add_by (classify_at_rank td desired_rank) !m;
          outmap := IntMap.add
            desired_rank
            (TIAMR.to_pairs (!m))
            !outmap
        done;
        f prn pq !outmap)
      (Placerun.get_pqueries pr)
  with
  | Placement.No_classif ->
    invalid_arg
      ((Placerun.get_name pr)^" contains unclassified queries!")

(* From an bootstrap map, produce a full classification map. *)
let nbc_classify td boot_map =
  let outmap = ref IntMap.empty
  and m = ref boot_map
  and n_ranks = Tax_taxonomy.get_n_ranks td in
  for desired_rank=(n_ranks-1) downto 0 do
    m := keymap_add_by (classify_at_rank td desired_rank) !m;
    outmap := IntMap.add
      desired_rank
      (TIAMR.to_pairs (!m))
      !outmap
  done;
  !outmap

(* From a classificication map, find the best classifications for each rank. *)
let best_classifications td ?multiclass_min cutoff m =
  let best_per_rank = IntMap.mapi
    (fun rank l ->
      (* This is the equivalent to filtering by `rank = desired_rank` in the
       * old best_classifications view. *)
      let l = List.filter (fst |- Tax_taxonomy.get_tax_rank td |- (=) rank) l in
      try
        (* If there's a clear best, pick that. *)
        List.find_map
          (fun (_, l as t) -> if l >= cutoff then Some [t] else None)
          l
        |> some
      with Not_found ->
        match multiclass_min with
        | None -> None
        | Some multiclass_min ->
          (* Otherwise, if we're multiclassifying, see if it adds up. *)
          let cl, sum = List.fold_left
            (fun (cl, sum as accum) (_, l as t) ->
              if l >= multiclass_min then t :: cl, sum +. l else accum)
            ([], 0.)
            l
          in
          if sum >= cutoff then Some cl else None)
    m
  in
  (* For every rank, find the first rank at or above it with a valid set of
   * classifications. *)
  let rec aux rank =
    match IntMap.Exceptionless.find rank best_per_rank with
    | Some Some cl -> rank, cl
    | _ when rank = 0 -> rank, []
    | _ -> aux (rank - 1)
  in
  IntMap.mapi (fun want_rank _ -> aux want_rank) m

(* Merge pplacer and nbc classifications, preferring pplacer. *)
let merge_pplacer_nbc_best_classif pp_id nbc_id _ pp nbc = match pp, nbc with
  | None, None -> None
  | None, Some nbc -> Some (nbc, nbc_id)
  | Some (pp_rank, _), Some ((nbc_rank, _) as nbc) when nbc_rank > pp_rank ->
    Some (nbc, nbc_id)
  | Some pp, _ -> Some (pp, pp_id)

let add_id id m =
  IntMap.map (fun x -> x, id) m

let merge_pplacer_nbc _ pp nbc = match pp, nbc with
  | None, None -> None
  | Some (x_id, x), None
  | None, Some (x_id, x) -> Some (add_id x_id x)
  | Some (pp_id, pp), Some (nbc_id, nbc) ->
    Some (IntMap.merge (merge_pplacer_nbc_best_classif pp_id nbc_id) pp nbc)

(* UI-related *)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit sqlite_cmd () as super_sqlite

  val classifier = flag "--classifier"
    (Formatted ("pplacer", "Which classifier to use, out of 'pplacer', 'nbc', 'hybrid', or 'rdp'. default: %s"))
  val cutoff = flag "--cutoff"
    (Formatted (0.9, "The default value for the likelihood_cutoff param. Default: %0.2f"))
  val bayes_cutoff = flag "--bayes-cutoff"
    (Formatted (1., "The default value for the bayes_cutoff param. Default: %0.2f"))
  val multiclass_min = flag "--multiclass-min"
    (Formatted (0.2, "The default value for the multiclass_min param. Default: %0.2f"))
  val bootstrap_min = flag "--bootstrap-min"
    (Formatted (0.8, "The default value for the bootstrap_min param. Default: %0.2f"))

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
  val target_rank = flag "--target-rank"
    (Formatted ("genus", "The desired most specific rank for NBC classification. default: %s"))
  val n_boot = flag "--n-boot"
    (Formatted (100, "The number of times to bootstrap a sequence with the NBC classifier. 0 = no bootstrap. default: %d"))
  val children = flag "-j"
    (Formatted (2, "The number of processes to spawn to do NBC classification. default: %d"))

  val rdp_results = flag "--rdp-results"
    (Needs_argument ("rdp results", "The RDP results file for use with the RDP classifier. \
                                     Can be specified multiple times for multiple inputs."))

  method specl =
    super_refpkg#specl
  @ super_sqlite#specl
  @ [
    string_flag classifier;
    float_flag cutoff;
    float_flag bayes_cutoff;
    float_flag multiclass_min;
    float_flag bootstrap_min;
    toggle_flag use_pp;
    string_flag tax_identity;
    toggle_flag mrca_class;
    delimited_list_flag nbc_sequences;
    int_flag word_length;
    string_flag target_rank;
    int_flag n_boot;
    int_flag children;
    delimited_list_flag rdp_results;
 ]

  method desc =
    "outputs classification information in SQLite format"
  method usage = "usage: classify [options] placefile[s]"

  method private placefile_action prl =
    let rp = self#get_rp in
    let criterion = if (fv use_pp) then Placement.post_prob else Placement.ml_ratio in
    let td = Refpkg.get_taxonomy rp in

    let do_pplacer, do_nbc, do_rdp = match fv classifier with
      | "pplacer" -> true, false, false
      | "nbc" -> false, true, false
      | "hybrid" -> true, true, false
      | "rdp" -> false, false, true
      | s -> failwith (Printf.sprintf "invalid classifier: %s" s)
    in

    let bayes_factors = Bayes_factor.of_refpkg rp (fv mrca_class) criterion in
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

    let best_nbc = if not do_nbc then None else
      let target_rank = fv target_rank
      and n_boot = fv n_boot
      and children = fv children
      and bootstrap_min = fv bootstrap_min in
      let rank_idx =
        try
          Tax_taxonomy.get_rank_index td target_rank
        with Not_found ->
          failwith (Printf.sprintf "invalid rank %s" target_rank)
      in
      let classif =
        Nbc.Classifier.of_refpkg ~n_boot (fv word_length) rank_idx rp
      in
      let bootstrap = Alignment.ungap
        |- Nbc.Classifier.bootstrap classif
        |- nbc_classify td
      and pn_st = Sqlite3.prepare db
        "INSERT INTO placement_names VALUES (?, ?, ?, 1);"
      and pc_st = Sqlite3.prepare db
        "INSERT INTO placement_nbc VALUES (?, ?, ?, ?, ?)"
      in

      let classify origin name boot_map accum =
        let place_id = new_place_id "nbc" in
        Sql.bind_step_reset db pn_st [|
          Sql.D.INT place_id;
          Sql.D.TEXT name;
          Sql.D.TEXT origin;
        |];
        flip IntMap.iter boot_map (fun rank_idx likelihoods ->
          let desired_rank = Tax_taxonomy.get_rank_name td rank_idx in
          flip List.iter likelihoods (fun (ti, likelihood) ->
            Sql.bind_step_reset db pc_st [|
              Sql.D.INT place_id;
              Sql.D.TEXT desired_rank;
              Sql.D.TEXT (Tax_taxonomy.rank_name_of_tax_id td ti);
              Sql.D.TEXT (Tax_id.to_string ti);
              Sql.D.FLOAT likelihood
        |]));
        StringMap.add
          name
          (place_id, best_classifications td bootstrap_min boot_map)
          accum
      in

      List.fold_left
        (fun accum infile ->
          Alignment.upper_aln_of_any_file infile
            |> Array.enum
            |> Enum.map (fun (name, seq) -> infile, name, seq)
            |> Enum.fold (flip List.cons) accum)
        []
        (fv nbc_sequences)
      |> Multiprocessing.map
          ~children
          ~progress_handler:(dprintf "%s\n")
          (tap (Tuple3.second |- dprintf "classifying %s...\n")
           |- Tuple3.map3 bootstrap)
      |> List.fold_left (Tuple3.uncurry classify |> flip) StringMap.empty
      |> some
    in

    let best_pplacer = if not do_pplacer then None else
      let pn_st = Sqlite3.prepare db
        "INSERT INTO placement_names VALUES (?, ?, ?, ?);"
      and pc_st = Sqlite3.prepare db
        "INSERT INTO placement_classifications VALUES (?, ?, ?, ?, ?)"
      and pp_st = Sqlite3.prepare db
        "INSERT INTO placement_positions VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      and pe_st = Sqlite3.prepare db
        "INSERT INTO placement_evidence VALUES (?, ?, ?, ?)"
      and best_classif_map = ref StringMap.empty
      and multiclass_min = fv multiclass_min
      and cutoff = fv cutoff in

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
        IntMap.iter
          (fun desired_rank rankl ->
            List.iter
              (fun (tax_id, prob) -> Sql.bind_step_reset db pc_st [|
                Sql.D.INT place_id;
                Sql.D.TEXT (Tax_taxonomy.get_rank_name td desired_rank);
                Sql.D.TEXT (Tax_taxonomy.rank_name_of_tax_id td tax_id);
                Tax_id.to_sql tax_id;
                Sql.D.FLOAT prob;
              |])
              rankl)
          rank_map;
        List.iter
          (fun p -> Sql.bind_step_reset db pp_st [|
            Sql.D.INT place_id;
            Sql.D.INT (Int64.of_int (Placement.location p));
            Sql.D.FLOAT (Placement.ml_ratio p);
            Sql.D.FLOAT (Placement.log_like p);
            Sql.D.FLOAT (Placement.distal_bl p);
            Sql.D.FLOAT (Placement.pendant_bl p);
            Placement.classif p |> Tax_id.to_sql;
            (match Placement.map_identity_opt p with
              | None -> Sql.D.NULL
              | Some (ratio, _) -> Sql.D.FLOAT ratio);
            (match Placement.map_identity_opt p with
              | None -> Sql.D.NULL
              | Some (_, denom) -> Sql.D.INT (Int64.of_int denom));
          |])
          (Pquery.place_list pq);
        Array.iter
          (fun (rank, ev, bf) -> Sql.bind_step_reset db pe_st [|
            Sql.D.INT place_id;
            Sql.D.TEXT rank;
            Sql.D.FLOAT ev;
            (match bf with
              | None -> Sql.D.NULL
              | Some bf -> Sql.D.FLOAT bf);
          |])
          (bayes_factors pq);
        let bc = best_classifications td ~multiclass_min cutoff rank_map in
        List.fold_left
          (flip StringMap.add (place_id, bc) |> flip)
          !best_classif_map
          (Pquery.namel pq)
        |> (:=) best_classif_map;
        tax_identity_func place_id pq;
      in
      List.iter (pplacer_classify Placement.classif criterion td classify) prl;
      Some (!best_classif_map)
    in

    let best_rdp = if not do_rdp then None else
      let name_map = Tax_id.TaxIdMap.enum td.Tax_taxonomy.tax_name_map
        |> Enum.map (curry identity |> flip |> uncurry |- second Tax_id.to_string)
        |> StringMap.of_enum
      and pn_st = Sqlite3.prepare db
        "INSERT INTO placement_names VALUES (?, ?, ?, 1.);"
      and pc_st = Sqlite3.prepare db
        "INSERT INTO placement_nbc VALUES (?, ?, ?, ?, ?)"
      and best_classif_map = ref StringMap.empty
      and bootstrap_min = fv bootstrap_min in

      let process origin name rows =
        let place_id = new_place_id "rdp" in
        Sql.bind_step_reset db pn_st
          [|
            Sql.D.INT place_id;
            Sql.D.TEXT name;
            Sql.D.TEXT origin;
          |];
        List.iter
          (Array.map (fun x -> Sql.D.TEXT x)
              |- Array.append [| Sql.D.INT place_id |]
              |- Sql.bind_step_reset db pc_st)
          rows;

        let class_map = List.fold_left
          (fun accum arr ->
            TIAMR.add_by
              (Tax_id.of_string arr.(2))
              (float_of_string arr.(3))
              accum)
          TIAMR.empty
          rows
        |> nbc_classify td
        in
        StringMap.add
          name
          (place_id, best_classifications td bootstrap_min class_map)
          !best_classif_map
        |> (:=) best_classif_map

      and classify line =
        (* past participle of 'to split' *)
        let splut = String.nsplit line "\t" |> Array.of_list in
        splut.(0), List.fold_left
          (fun accum idx ->
            [|
              splut.(idx + 1);
              splut.(idx + 1);
              (try
                 StringMap.find splut.(idx) name_map
               with Not_found ->
                 failwith (splut.(idx)^" not found in refpkg's taxonomy"));
              splut.(idx + 2);
            |] :: accum)
          []
          [8; 11; 14; 17; 20]

      in
      fv rdp_results
        |> List.enum
        |> Enum.map
            (identity &&& (File.lines_of |- Enum.map classify))
        |> Enum.iter (fun (a, bcl) -> Enum.iter (process a |> uncurry) bcl);

      Some !best_classif_map
    in

    let multiclass = match best_pplacer, best_nbc, best_rdp with
      | Some x, None, None
      | None, Some x, None
      | None, None, Some x -> StringMap.merge merge_pplacer_nbc x StringMap.empty
      | Some pp, Some nbc, None -> StringMap.merge merge_pplacer_nbc pp nbc
      | _ -> invalid_arg "multiclass"
    and mc_st = Sqlite3.prepare db
      "INSERT INTO multiclass VALUES (?, ?, ?, ?, ?, ?)"
    in
    flip StringMap.iter multiclass (fun seq_name rank_map ->
      flip IntMap.iter rank_map (fun want_rank ((rank, cl), pid) ->
        flip List.iter cl (fun (ti, l) -> Sql.bind_step_reset db mc_st [|
          Sql.D.INT pid;
          Sql.D.TEXT seq_name;
          Sql.D.TEXT (Tax_taxonomy.get_rank_name td want_rank);
          Sql.D.TEXT (Tax_taxonomy.get_rank_name td rank);
          Sql.D.TEXT (Tax_id.to_string ti);
          Sql.D.FLOAT l;
        |])));

    Sql.check_exec db "COMMIT";
    Sql.close db

end


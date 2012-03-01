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

(* classify a pquery at all ranks, returning a string list list representing
 * the table of classifications. *)
let classif_strll td pq rank_map =
  List.fold_left
    (fun accum name ->
      List.rev_append
        (IntMap.fold
           (fun desired_rank rankl accum ->
             List.rev_append
               (List.fold_left
                  (fun accum (ti, p) ->
                    [
                      name;
                      Tax_taxonomy.get_rank_name td desired_rank;
                      Tax_taxonomy.rank_name_of_tax_id td ti;
                      Tax_id.to_string ti;
                      Printf.sprintf "%g" p;
                    ] :: accum)
                  []
                  rankl)
               accum)
           rank_map
           [])
        accum)
    []
    (Pquery.namel pq)

(* Write out classifications.
 * f is the function used to write out the classification.
 * Return the number of placements that are just proximal of an MRCA (these are
 * the ones that are difficult to classify.) *)
let full_classify td boot_map =
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

(* UI-related *)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit sqlite_cmd () as super_sqlite
  inherit tabular_cmd () as super_tabular

  val word_length = flag "--word-length"
    (Formatted (8, "The length of the words used for NBC classification. default: %d"))
  val target_rank = flag "--target-rank"
    (Formatted ("genus", "The desired most specific rank for classification. default: %s"))
  val boot_rows = flag "--boot-rows"
    (Formatted (100, "The number of times to bootstrap a sequence. default: %d"))

  method specl =
    super_refpkg#specl
  @ super_sqlite#specl
  @ super_tabular#specl
  @ [
    int_flag word_length;
    string_flag target_rank;
    int_flag boot_rows;
  ]

  method desc =
    "outputs classification information in a tabular or SQLite format"
  method usage = "usage: classify [options] placefile[s]"

  method action inl =
    let rp = self#get_rp in
    let td = Refpkg.get_taxonomy rp
    and target_rank = fv target_rank
    and boot_rows = fv boot_rows
    and gt = Refpkg.get_ref_tree rp in
    let rank_idx =
      try
        Tax_taxonomy.get_rank_index td target_rank
      with Not_found ->
        failwith (Printf.sprintf "invalid rank %s" target_rank)
    in
    let rank_tax_map = Convex.rank_tax_map_of_refpkg rp in
    let preclassif = IntMap.find rank_idx rank_tax_map
      |> IntMap.values
      |> Tax_id.TaxIdSet.of_enum
      |> Tax_id.TaxIdSet.enum
      |> Array.of_enum
      |> Nbc.Preclassifier.make Bigarray.int (fv word_length)
    and seq_tax_ids = IntMap.find rank_idx rank_tax_map
      |> IntMap.enum
      |> Enum.map (first (Gtree.get_node_label gt))
      |> StringMap.of_enum
    and filter m (k, seq) =
      match StringMap.Exceptionless.find k m with
        | Some v -> Some (v, Nbc.filter_informative seq)
        | None -> None
    in
    Refpkg.get_aln_fasta rp
      |> Array.enum
      |> Enum.filter_map (filter seq_tax_ids)
      |> Enum.iter (uncurry (Nbc.Preclassifier.add_seq preclassif succ));
    let classif = Nbc.Classifier.make preclassif ~boot_rows (+) float_of_int in
    let bootstrap = Nbc.filter_informative
      |- Nbc.Classifier.bootstrap classif
      |- full_classify td
    and db = self#get_db in
    Sql.check_exec
      db
      ~cb:(fun row _ -> match row with
      | [| Some "1" |] -> ()
      | _ -> failwith "run `rppr prep_db` before running `guppy nbc`")
      "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE name = 'placement_classifications')";
    Sql.check_exec db "BEGIN TRANSACTION";
    let pn_st = Sqlite3.prepare db
      "INSERT INTO placement_names VALUES (?, ?, ?, 1);"
    and pc_st = Sqlite3.prepare db
      "INSERT INTO placement_classifications VALUES (?, ?, ?, ?, ?)"
    in

    let classify origin name seq =
      dprintf "classifying %s\n" name;
      Sql.check_exec db "INSERT INTO placements VALUES (NULL)";
      let place_id = Sqlite3.last_insert_rowid db in
      Sql.bind_step_reset db pn_st [|
        Sql.D.INT place_id;
        Sql.D.TEXT name;
        Sql.D.TEXT origin;
      |];
      bootstrap seq
      |> IntMap.iter (fun rank_idx likelihoods ->
        let desired_rank = Tax_taxonomy.get_rank_name td rank_idx in
        flip List.iter likelihoods (fun (ti, likelihood) ->
          Sql.bind_step_reset db pc_st [|
            Sql.D.INT place_id;
            Sql.D.TEXT desired_rank;
            Sql.D.TEXT (Tax_taxonomy.rank_name_of_tax_id td ti);
            Sql.D.TEXT (Tax_id.to_string ti);
            Sql.D.FLOAT likelihood
          |]))
    in

    flip List.iter inl (fun infile ->
      Alignment.upper_aln_of_any_file infile
      |> Array.iter (fun (name, seq) -> classify infile name seq));
    Sql.check_exec db "COMMIT";
    Sql.close db

end

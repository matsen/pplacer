open Subcommand
open Guppy_cmdobjs
open Ppatteries

module TIAMR = AlgMapR (Tax_id.TaxIdMap)
module GC = Guppy_classify

let full_classify td boot_map =
  let outmap = ref IntMap.empty
  and m = ref boot_map
  and n_ranks = Tax_taxonomy.get_n_ranks td in
  for desired_rank=(n_ranks-1) downto 0 do
    m := GC.keymap_add_by (GC.classify_at_rank td desired_rank) !m;
    outmap := IntMap.add
      desired_rank
      (TIAMR.to_pairs (!m))
      !outmap
  done;
  !outmap

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
  val children = flag "-j"
    (Formatted (2, "The number of processes to spawn to do classification. default: %d"))

  method specl =
    super_refpkg#specl
  @ super_sqlite#specl
  @ super_tabular#specl
  @ [
    int_flag word_length;
    string_flag target_rank;
    int_flag boot_rows;
    int_flag children;
  ]

  method desc = "classifies sequences with a naive Bayes classifier"
  method usage = "usage: nbc [options] query-sequence-file[s]"

  method action inl =
    let rp = self#get_rp in
    let td = Refpkg.get_taxonomy rp
    and target_rank = fv target_rank
    and boot_rows = fv boot_rows
    and children = fv children in
    let rank_idx =
      try
        Tax_taxonomy.get_rank_index td target_rank
      with Not_found ->
        failwith (Printf.sprintf "invalid rank %s" target_rank)
    in
    let classif =
      Nbc.Classifier.of_refpkg ~boot_rows (fv word_length) rank_idx rp
    in
    let bootstrap = Alignment.ungap
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

    let classify origin name boot_map =
      Sql.check_exec db "INSERT INTO placements VALUES (NULL)";
      let place_id = Sqlite3.last_insert_rowid db in
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
          |]))
    in

    List.fold_left
      (fun accum infile ->
        Alignment.upper_aln_of_any_file infile
          |> Array.enum
          |> Enum.map (fun (name, seq) -> infile, name, seq)
          |> Enum.fold (flip List.cons) accum)
      []
      inl
    |> tap
        (fun l ->
          dprintf "starting classification of %d sequences.\n" (List.length l))
    |> Multiprocessing.map
        ~children
        ~progress_handler:(dprintf "%s\n")
        (tap (Tuple3.second |- dprintf "classifying %s...\n")
         |- Tuple3.map3 bootstrap)
    |> List.iter (Tuple3.uncurry classify);
    Sql.check_exec db "COMMIT";
    Sql.close db

end

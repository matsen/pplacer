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

(* m is a taxid_algmap and this outputs a list of string lists, one for each
 * placement *)
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
let classify how criterion n_ranks td pr f mrca_map =
  let is_key id m =
    try let _ = IntMap.find id m in true with
    | Not_found -> false
  in
  (* Does this placement have some mass on an edge just proximal of an MRCA? *)
  let has_mrca_proximal_mass pq =
    List.fold_left
      (fun accum p -> accum || is_key (Placement.location p) mrca_map)
      false
      (Pquery.place_list pq)
  in
  try
    List.fold_left
      (fun n_proximal pq ->
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
        f pq (!outmap);
        n_proximal + if has_mrca_proximal_mass pq then 1 else 0)
      0
      (Placerun.get_pqueries pr)
  with
    | Placement.No_classif ->
      invalid_arg
        ((Placerun.get_name pr)^" contains unclassified queries!")

let median l =
  let rec aux = function
    | e :: _, ([_] | [_; _]) -> e
    | _ :: tl1, _ :: _ :: tl2 -> aux (tl1, tl2)
    | _, _ -> invalid_arg "median"
  in
  aux (l, l)


(* UI-related *)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit sqlite_cmd () as super_sqlite
  inherit tabular_cmd () as super_tabular

  val use_pp = flag "--pp"
    (Plain (false, "Use posterior probability for our criteria."))
  val mrca_stats = flag "--mrca-stats"
    (Plain (false, "Print the number of placements just proximal to MRCAs."))
  val tax_identity = flag "--tax-median-identity-from"
    (Needs_argument ("", "Calculate the median identity for each sequence per-tax_id from the specified alignment."))
  val mrca_class = flag "--mrca-class"
    (Plain (false, "Classify against a placefile that was generated with MRCA classification"))

  method specl =
    super_refpkg#specl
  @ super_sqlite#specl
  @ [
    toggle_flag use_pp;
    toggle_flag mrca_stats;
    string_flag tax_identity;
    toggle_flag mrca_class;
  ]

  method desc =
    "outputs classification information in a tabular or SQLite format"
  method usage = "usage: classify [options] placefile[s]"

  method private placefile_action prl =
    let rp = self#get_rp in
    let criterion = if (fv use_pp) then Placement.post_prob else Placement.ml_ratio in
    let td = Refpkg.get_taxonomy rp in
    let n_ranks = Tax_taxonomy.get_n_ranks td in
    let sqlite_out = match !(sqlite_fname.Subcommand.value) with
      | Some _ -> true
      | None -> false
    in
    let out_func pr =
      if sqlite_out then
        let bayes_factors = Bayes_factor.of_refpkg rp (fv mrca_class) criterion in
        let prn = Placerun.get_name pr in
        let db = self#get_db in
        let close () =
          Sql.check_exec db "COMMIT";
          Sql.close db
        in
        Sql.check_exec
          db
          ~cb:(fun row _ -> match row with
            | [| Some "1" |] -> ()
            | _ -> failwith "run `rppr prep_db` before running `guppy classify`")
          "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE name = 'placement_classifications')";
        Sql.check_exec db "BEGIN TRANSACTION";
        let pn_st = Sqlite3.prepare db
          "INSERT INTO placement_names VALUES (?, ?, ?);"
        and pc_st = Sqlite3.prepare db
          "INSERT INTO placement_classifications VALUES (?, ?, ?, ?, ?)"
        and pp_st = Sqlite3.prepare db
          "INSERT INTO placement_positions VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        and pe_st = Sqlite3.prepare db
          "INSERT INTO placement_evidence VALUES (?, ?, ?, ?)"
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
                    Sql.D.TEXT (to_string ti);
                    Sql.D.FLOAT
                      (List.map id seql |> List.sort compare |> median);
                  |])
                refs_by_tax_id;
        in

        close, begin fun pq rank_map ->
          Sql.check_exec db "INSERT INTO placements VALUES (NULL)";
          let place_id = Sqlite3.last_insert_rowid db in
          List.iter
            (fun name -> Sql.bind_step_reset db pn_st [|
                Sql.D.INT place_id;
                Sql.D.TEXT name;
                Sql.D.TEXT prn;
              |])
            (Pquery.namel pq);
          IntMap.iter
            (fun desired_rank rankl ->
              List.iter
                (fun (tax_id, prob) -> Sql.bind_step_reset db pc_st [|
                  Sql.D.INT place_id;
                  Sql.D.TEXT (Tax_taxonomy.get_rank_name td desired_rank);
                  Sql.D.TEXT (Tax_taxonomy.rank_name_of_tax_id td tax_id);
                  Sql.D.TEXT (Tax_id.to_string tax_id);
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
              Sql.D.TEXT (Tax_id.to_string (Placement.classif p));
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

          tax_identity_func place_id pq;

        end;

      else
        let prn = Placerun.get_name pr in
        let ch =
          prn ^ ".class" ^ (if fv as_csv then ".csv" else ".tab")
            |> open_out
        and rows = ref [] in
        (fun () ->
          !rows
            |> List.cons ["name"; "desired_rank"; "rank";
                          "tax_id"; "likelihood"; "origin"]
            |> self#write_ll_tab ~ch),
        (fun pq rank_map ->
          classif_strll td pq rank_map
            |> List.map (flip List.append [prn])
            |> List.append !rows
            |> (:=) rows)

    in
    let mrcam = Refpkg.get_mrcam rp in
    if fv mrca_stats then Printf.printf "name\tn_prox\tn_classified\n";
    List.iter
      (fun pr ->
        let close, out_func = out_func pr in
        let n_proximal =
          classify Placement.classif criterion n_ranks td pr out_func mrcam
        in
        if fv mrca_stats then
          Printf.printf
            "%s\t%d\t%d\n"
            (Placerun.get_name pr)
            n_proximal
            (Placerun.n_pqueries pr);
        close ())
      prl

end


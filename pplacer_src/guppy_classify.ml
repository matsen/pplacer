open Subcommand
open Guppy_cmdobjs
open MapsSets

let escape = Base.sqlite_escape
module TIAMR = AlgMap.AlgMapR (Tax_id.TaxIdMap)

(* if rank is less than the tax rank of ti, then move up the taxonomy until
 * the first time that the tax rank is less than or equal to rank *)
let classify_at_rank td rank ti =
  let rec aux curr_ti =
    if rank >= Tax_taxonomy.get_tax_rank td curr_ti then curr_ti
    else
      aux
        (try Tax_taxonomy.get_ancestor td curr_ti with
        | Tax_taxonomy.NoAncestor _ -> assert(false))
  in
  aux ti

(* apply f to all of the keys and add the results together *)
let keymap_add_by f m =
  List.fold_right
    (fun (k,v) -> (TIAMR.add_by (f k) v))
    (TIAMR.to_pairs m)
    TIAMR.empty

(* m is a taxid_algmap and this outputs a list of string_arrays, one for each
 * placement *)
let classif_stral td pq rank_map =
  List.fold_left
    (fun accum name ->
      List.rev_append
        (IntMap.fold
           (fun desired_rank rankl accum ->
             List.rev_append
               (List.fold_left
                  (fun accum (ti, p) ->
                    [|
                      name;
                      Tax_taxonomy.get_rank_name td desired_rank;
                      Tax_taxonomy.rank_name_of_tax_id td ti;
                      Tax_id.to_string ti;
                      Printf.sprintf "%g" p;
                    |] :: accum)
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


(* UI-related *)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit sqlite_cmd () as super_sqlite

  val use_pp = flag "--pp"
    (Plain (false, "Use posterior probability for our criteria."))
  val csv_out = flag "--csv"
    (Plain (false, "Write .class.csv files containing CSV data."))
  val mrca_stats = flag "--mrca-stats"
    (Plain (false, "Print the number of placements just proximal to MRCAs."))

  method specl =
    super_refpkg#specl
  @ super_sqlite#specl
  @ [
    toggle_flag use_pp;
    toggle_flag csv_out;
    toggle_flag mrca_stats;
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
      if fv csv_out then
        let prn = Placerun.get_name pr in
        let ch = open_out (prn ^ ".class.csv") in
        let close () = close_out ch in
        output_string ch "name,desired_rank,rank,tax_id,likelihood,origin\n";
        close, (fun pq rank_map ->
          let outl = classif_stral td pq rank_map in
          Csv.save_out
            ch
            (List.map
               (fun arr -> (Array.to_list arr) @ [prn])
               outl))

      else if sqlite_out then
        let prn = Placerun.get_name pr in
        let db = self#get_db in
        let close () =
          Sql.check_exec db "COMMIT";
          Sql.close db
        in
        Sql.check_exec db "BEGIN TRANSACTION";
        let pn_st = Sqlite3.prepare db
          "INSERT INTO placement_names VALUES (?, ?, ?);"
        and pc_st = Sqlite3.prepare db
          "INSERT INTO placement_classifications VALUES (?, ?, ?, ?, ?)"
        in
        close, (fun pq rank_map ->
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
            rank_map)

      else
        let ch = open_out ((Placerun.get_name pr)^".class.tab") in
        let close () = close_out ch in
        close, (fun pq rank_map ->
          String_matrix.write_padded ch (Array.of_list (classif_stral td pq rank_map)))

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


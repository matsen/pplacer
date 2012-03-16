open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit sqlite_cmd () as super_sqlite

  val cutoff = flag "--cutoff"
    (Formatted (0.8, "The bootstrap cutoff for which classifications are considered best. default: %g"))

  method specl =
     float_flag cutoff
  :: super_refpkg#specl
   @ super_sqlite#specl

  method desc = "converts RDP output to something resmbling guppy classify output"
  method usage = "usage: classify_rdp -c some.refpkg rdp_output_file[s]"

  method action argl =
    let rp = self#get_rp in
    let td = Refpkg.get_taxonomy rp in
    let name_map = Tax_id.TaxIdMap.enum td.Tax_taxonomy.tax_name_map
      |> Enum.map (curry identity |> flip |> uncurry |- second Tax_id.to_string)
      |> StringMap.of_enum
    and cutoff = fv cutoff in

    let db = self#get_db in
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

    let pn_st = Sqlite3.prepare db
      "INSERT INTO placement_names VALUES (?, ?, ?, 1.);"
    and pc_st = Sqlite3.prepare db
      "INSERT INTO placement_nbc VALUES (?, ?, ?, ?, ?)"
    and mc_st = Sqlite3.prepare db
      "INSERT INTO multiclass VALUES (?, ?, ?, ?, ?, ?)"
    in

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
      List.iter
        (junction
           (fun arr -> float_of_string arr.(3) < cutoff)
           (const ())
           (Array.map (fun x -> Sql.D.TEXT x)
            |- Array.append [| Sql.D.INT place_id; Sql.D.TEXT name |]
            |- Sql.bind_step_reset db mc_st))
        rows;

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
    List.enum argl
      |> Enum.map
          (identity &&& (File.lines_of |- Enum.map classify))
      |> Enum.iter (fun (a, bcl) -> Enum.iter (process a |> uncurry) bcl);

    Sql.check_exec db "COMMIT";
    Sql.close db

end

open Batteries
open Subcommand
open Guppy_cmdobjs
open MapsSets

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit sqlite_cmd () as super_sqlite

  val csv_out = flag "--csv"
    (Plain (false, "Write .class.csv files containing CSV data."))

  method specl = toggle_flag csv_out :: super_refpkg#specl @ super_sqlite#specl

  method desc = "converts RDP output to something resmbling guppy classify output"
  method usage = "usage: classify_rdp -c some.refpkg rdp_output_file[s]"

  method action argl =
    let rp = self#get_rp in
    let td = Refpkg.get_taxonomy rp in
    let name_map = Tax_id.TaxIdMap.enum td.Tax_taxonomy.tax_name_map
      |> Enum.map (curry identity |> flip |> uncurry |- second Tax_id.to_string)
      |> StringMap.of_enum
    and sqlite_out = match !(sqlite_fname.Subcommand.value) with
      | Some _ -> true
      | None -> false
    in

    let out_func =
      if sqlite_out then begin
        let db = self#get_db in
        let close () =
          Sql.check_exec db "COMMIT";
          Sql.close db
        and process name =
          Sql.check_exec db "BEGIN TRANSACTION";
          let pn_st = Sqlite3.prepare db
            "INSERT INTO placement_names VALUES (?, ?, ?);"
          and pc_st = Sqlite3.prepare db
            "INSERT INTO placement_classifications VALUES (?, ?, ?, ?, ?)"
          in

          Sql.check_exec db "INSERT INTO placements VALUES (NULL)";
          let place_id = Sqlite3.last_insert_rowid db in
          Enum.iter
            (fun arr ->
              Sql.bind_step_reset db pn_st
                [|
                  Sql.D.INT place_id;
                  Sql.D.TEXT arr.(0);
                  Sql.D.TEXT name;
                |];
              Sql.bind_step_reset db pc_st
                [|
                  Sql.D.INT place_id;
                  Sql.D.TEXT arr.(1);
                  Sql.D.TEXT arr.(2);
                  Sql.D.TEXT arr.(3);
                  Sql.D.TEXT arr.(4);
                |])
        in
        finally close process

      end else if fv csv_out then begin fun name ->
        let ch = Legacy.open_out (name ^ ".class.csv") in
        Csv.save_out
          ch
          [["name"; "desired_rank"; "rank"; "tax_id"; "likelihood"; "origin"]];
        Enum.map Array.to_list
          |- List.of_enum
          |- Csv.save_out ch
          |> finally (fun () -> Legacy.close_out ch)

      end else begin fun name ->
        let ch = Legacy.open_out (name ^ ".class.tab") in
        Array.of_enum
          |- String_matrix.write_padded ch
          |> finally (fun () -> Legacy.close_out ch)

      end

    and classify line =
      (* past participle of 'to split' *)
      let splut = String.nsplit line "\t" |> Array.of_list in
      List.fold_left
        (fun accum idx ->
          [|
            splut.(0);
            splut.(idx + 1);
            splut.(idx + 1);
            (MapsSets.StringMap.find splut.(idx) name_map);
            splut.(idx + 2);
          |] :: accum)
        []
        [8; 11; 14; 17; 20]

    in
    List.enum argl
      |> Enum.map
          (identity &&& (File.lines_of |- Enum.map (classify |- List.enum) |- Enum.flatten))
      |> Enum.iter (uncurry out_func)

end

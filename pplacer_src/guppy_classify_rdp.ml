open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit sqlite_cmd () as super_sqlite
  inherit output_cmd ~prefix_required:true () as super_output

  val csv_out = flag "--csv"
    (Plain (false, "Write .class.csv files containing CSV data."))

  method specl =
     toggle_flag csv_out
  :: super_refpkg#specl
   @ super_sqlite#specl
   @ super_output#specl

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

    let open_out name =
      (self#single_prefix ()) ^ (Filename.basename name) |> open_out
    in

    let out_func =
      if sqlite_out then begin
        let db = self#get_db in
        Sql.check_exec db "BEGIN TRANSACTION";
        let pn_st = Sqlite3.prepare db
          "INSERT INTO placement_names VALUES (?, ?, ?);"
        and pc_st = Sqlite3.prepare db
          "INSERT INTO placement_classifications VALUES (?, ?, ?, ?, ?)"
        in
        let close () =
          Sql.check_exec db "COMMIT";
          Sql.close db
        and process origin name =
          Sql.check_exec db "INSERT INTO placements VALUES (NULL)";
          let place_id = Sqlite3.last_insert_rowid db in
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

        in
        finally close process

      end else if fv csv_out then begin fun origin ->
        let ch = open_out origin in
        let csvch = csv_out_channel ch |> Csv.to_out_obj in
        Csv.output_record
          csvch
          ["name"; "origin"; "desired_rank"; "rank"; "tax_id"; "likelihood"];
        fun name -> List.map (Array.append [| name; origin |] |- Array.to_list)
          |- Csv.output_all csvch
          |> finally (fun () -> close_out ch)

      end else begin fun origin ->
        let ch = open_out origin in
        fun name -> List.map (Array.append [| name; origin |])
          |- Array.of_list
          |- String_matrix.write_padded ch
          |> finally (fun () -> close_out ch)

      end

    and classify line =
      (* past participle of 'to split' *)
      let splut = String.nsplit line "\t" |> Array.of_list in
      splut.(0), List.fold_left
        (fun accum idx ->
          [|
            splut.(idx + 1);
            splut.(idx + 1);
            (try StringMap.find splut.(idx) name_map with
              Not_found -> failwith (splut.(idx)^" not found in rdp classify"));
            splut.(idx + 2);
          |] :: accum)
        []
        [8; 11; 14; 17; 20]

    in
    List.enum argl
      |> Enum.map
          (identity &&& (File.lines_of |- Enum.map classify))
      |> Enum.iter (fun (a, bcl) -> Enum.iter (out_func a |> uncurry) bcl)

end

open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit sqlite_cmd () as super_sqlite

  method specl =
    super_refpkg#specl
    @ super_sqlite#specl

  method desc = "makes SQL enabling taxonomic querying of placement results"
  method usage = "usage: prep_db [options] -c <refpkg>"

  method action _ =
    let refpkg = self#get_rp in
    let db = self#get_db in
    let tax = Refpkg.get_taxonomy refpkg in
    Sql.check_exec db "BEGIN TRANSACTION";
    Sql.check_exec db "
      CREATE TABLE ranks (
        rank TEXT PRIMARY KEY NOT NULL,
        rank_order INTEGER
      );

      CREATE TABLE taxa (
        tax_id TEXT PRIMARY KEY NOT NULL,
        tax_name TEXT NOT NULL,
        rank TEXT REFERENCES ranks (rank) NOT NULL
      );

      CREATE TABLE runs (
        run_id INTEGER PRIMARY KEY AUTOINCREMENT,
        params TEXT NOT NULL
      );

      CREATE TABLE placements (
        placement_id INTEGER PRIMARY KEY AUTOINCREMENT,
        classifier TEXT NOT NULL,
        run_id INTEGER REFERENCES runs (run_id) NOT NULL
      );

      CREATE TABLE placement_names (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        name TEXT NOT NULL,
        origin TEXT NOT NULL,
        mass REAL NOT NULL,
        PRIMARY KEY (placement_id, name, origin)
      );
      CREATE INDEX placement_names_id ON placement_names (placement_id);

      CREATE TABLE placement_classifications (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        desired_rank TEXT REFERENCES ranks (rank) NOT NULL,
        rank TEXT REFERENCES ranks (rank) NOT NULL,
        tax_id TEXT REFERENCES taxa (tax_id),
        likelihood REAL NOT NULL
      );
      CREATE INDEX placement_classifications_id ON placement_classifications (placement_id);

      CREATE TABLE placement_nbc (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        desired_rank TEXT REFERENCES ranks (rank) NOT NULL,
        rank TEXT REFERENCES ranks (rank) NOT NULL,
        tax_id TEXT REFERENCES taxa (tax_id),
        bootstrap REAL NOT NULL
      );
      CREATE INDEX placement_nbc_id ON placement_nbc (placement_id);

      CREATE TABLE placement_evidence (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        rank TEXT REFERENCES ranks (rank) NOT NULL,
        evidence REAL NOT NULL,
        bayes_factor REAL
      );
      CREATE INDEX placement_evidence_id ON placement_evidence (placement_id);

      CREATE TABLE placement_positions (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        location INTEGER NOT NULL,
        ml_ratio REAL NOT NULL,
        log_like REAL NOT NULL,
        distal_bl REAL NOT NULL,
        pendant_bl REAL NOT NULL,
        tax_id TEXT REFERENCES taxa (tax_id),
        map_identity_ratio REAL,
        map_identity_denom INTEGER
       );
      CREATE INDEX placement_positions_id ON placement_classifications (placement_id);

      CREATE TABLE placement_median_identities (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        tax_id TEXT REFERENCES taxa (tax_id) NOT NULL,
        median_percent_identity REAL NOT NULL,
        PRIMARY KEY (placement_id, tax_id)
      );
      CREATE INDEX placement_median_identities_id ON placement_median_identities (placement_id);

      CREATE TABLE multiclass (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        name TEXT NOT NULL,
        want_rank TEXT REFERENCES ranks (rank) NOT NULL,
        rank TEXT REFERENCES ranks (rank) NOT NULL,
        tax_id TEXT REFERENCES taxa (tax_id),
        likelihood REAL NOT NULL
      );
    ";

    let st = Sqlite3.prepare db "INSERT INTO ranks VALUES (?, ?)" in
    Array.iteri
      (fun idx name ->
        Sql.bind_step_reset db st [|
          Sql.D.TEXT name;
          Sql.D.INT (Int64.of_int idx)
        |])
      tax.Tax_taxonomy.rank_names;
    let st = Sqlite3.prepare db "INSERT INTO taxa VALUES (?, ?, ?)" in
    Tax_id.TaxIdMap.iter
      (fun tax_id name ->
        let rank = Tax_taxonomy.rank_name_of_tax_id tax tax_id in
        Sql.bind_step_reset db st [|
          Tax_id.to_sql tax_id;
          Sql.D.TEXT name;
          Sql.D.TEXT rank;
        |])
      tax.Tax_taxonomy.tax_name_map;
    Sql.check_exec db "COMMIT";
    Sql.close db
end

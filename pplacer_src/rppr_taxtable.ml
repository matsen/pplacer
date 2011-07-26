open Subcommand
open Guppy_cmdobjs

let escape = Base.sqlite_escape

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit sqlite_cmd () as super_sqlite

  val default_cutoff = flag "--default-cutoff"
    (Formatted (0.9, "The default value for the likelihood_cutoff table. Default: %0.2f"))

  method specl =
    super_refpkg#specl
    @ super_sqlite#specl
    @ [
      float_flag default_cutoff;
    ]

  method desc = "makes SQL enabling taxonomic querying of placement results"
  method usage = "usage: taxtable [options] -c <refpkg>"

  method action _ =
    let refpkg = self#get_rp in
    let db = self#get_db in
    let tax = Refpkg.get_taxonomy refpkg in
    Sql.check_exec db "BEGIN TRANSACTION";
    let st = Sqlite3.prepare db
      "CREATE TABLE likelihood_cutoff AS SELECT ? AS val;" in
    Sql.bind_step_reset db st [| Sql.D.FLOAT (fv default_cutoff) |];
    Sql.check_exec db "
      CREATE TABLE IF NOT EXISTS ranks (
        rank TEXT PRIMARY KEY NOT NULL,
        rank_order INTEGER
      );

      CREATE TABLE IF NOT EXISTS taxa (
        tax_id TEXT PRIMARY KEY NOT NULL,
        tax_name TEXT NOT NULL,
        rank TEXT REFERENCES ranks (rank) NOT NULL
      );

      CREATE TABLE IF NOT EXISTS placements (
        placement_id INTEGER PRIMARY KEY AUTOINCREMENT
      );

      CREATE TABLE IF NOT EXISTS placement_names (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        name TEXT NOT NULL UNIQUE,
        origin TEXT NOT NULL,
        PRIMARY KEY (placement_id, name, origin)
      );
      CREATE INDEX placement_names_id ON placement_names (placement_id);

      CREATE TABLE IF NOT EXISTS placement_classifications (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        desired_rank TEXT REFERENCES ranks (rank) NOT NULL,
        rank TEXT REFERENCES ranks (rank) NOT NULL,
        tax_id TEXT REFERENCES taxa (tax_id) NOT NULL,
        likelihood REAL NOT NULL
      );
      CREATE INDEX placement_classifications_id ON placement_classifications (placement_id);

      CREATE TABLE IF NOT EXISTS placement_positions (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        location INTEGER NOT NULL,
        ml_ratio REAL NOT NULL,
        log_like REAL NOT NULL,
        distal_bl REAL NOT NULL,
        pendant_bl REAL NOT NULL,
        tax_id TEXT REFERENCES taxa (tax_id) NOT NULL,
        map_identity_ratio REAL,
        map_identity_denom INTEGER
       );
      CREATE INDEX placement_positions_id ON placement_classifications (placement_id);

      CREATE VIEW best_classifications
      AS
        SELECT placement_id,
               tax_id,
               rank,
               likelihood
        FROM   (SELECT *
                FROM   placements
                       JOIN placement_classifications USING (placement_id)
                       JOIN ranks USING (rank)
                WHERE  rank = desired_rank
                       AND likelihood > (SELECT val FROM likelihood_cutoff)
                ORDER  BY placement_id,
                          rank_order ASC,
                          likelihood ASC)
        GROUP  BY placement_id;

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
          Sql.D.TEXT (Tax_id.to_string tax_id);
          Sql.D.TEXT name;
          Sql.D.TEXT rank;
        |])
      tax.Tax_taxonomy.tax_name_map;
    Sql.check_exec db "COMMIT";
    Sql.close db
end

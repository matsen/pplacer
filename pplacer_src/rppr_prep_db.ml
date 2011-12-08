open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit sqlite_cmd () as super_sqlite

  val default_cutoff = flag "--default-cutoff"
    (Formatted (0.9, "The default value for the likelihood_cutoff param. Default: %0.2f"))
  val default_count = flag "--default-multiclass-count"
    (Formatted (3, "The default value for the multiclass_count param. Default: %d"))
  val default_likelihood = flag "--default-multiclass-likelihood"
    (Formatted (0.05, "The default value for the multiclass_likelihood param. Default: %0.3f"))
  val default_bayes_cutoff = flag "--default-bayes-cutoff"
    (Formatted (1., "The default value for the bayes_cutoff param. Default: %0.2f"))

  method specl =
    super_refpkg#specl
    @ super_sqlite#specl
    @ [
      float_flag default_cutoff;
      float_flag default_bayes_cutoff;
    ]

  method desc = "makes SQL enabling taxonomic querying of placement results"
  method usage = "usage: prep_db [options] -c <refpkg>"

  method action _ =
    let refpkg = self#get_rp in
    let db = self#get_db in
    let tax = Refpkg.get_taxonomy refpkg in
    Sql.check_exec db "BEGIN TRANSACTION";
    Sql.check_exec db "
      CREATE TABLE params (
        name TEXT,
        val REAL
      );

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

      CREATE TABLE IF NOT EXISTS placement_evidence (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        rank TEXT REFERENCES ranks (rank) NOT NULL,
        evidence REAL NOT NULL,
        bayes_factor REAL
      );
      CREATE INDEX placement_evidence_id ON placement_evidence (placement_id);

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
        SELECT *
        FROM   placements
               LEFT JOIN (SELECT placement_id,
                                 tax_id,
                                 rank,
                                 likelihood
                          FROM   (SELECT *
                                  FROM   placements
                                         JOIN placement_classifications USING (placement_id)
                                         JOIN ranks USING (rank)
                                  WHERE  rank = desired_rank
                                         AND likelihood > (SELECT val
                                                           FROM   params
                                                           WHERE  name = 'likelihood_cutoff')
                                  ORDER  BY placement_id,
                                            rank_order ASC,
                                            likelihood ASC)
                          GROUP  BY placement_id) USING (placement_id);

      CREATE VIEW multiclass
      AS
        SELECT bc.placement_id,
               pc.tax_id,
               COALESCE(below_rank, bc.rank) AS rank,
               pc.likelihood
        FROM   best_classifications bc
               LEFT JOIN (SELECT *
                          FROM   (SELECT placement_id,
                                         bc.rank,
                                         pc.rank AS below_rank
                                  FROM   best_classifications bc
                                         JOIN placement_classifications pc USING (placement_id)
                                         JOIN ranks bcr
                                           ON bc.rank = bcr.rank
                                         JOIN ranks pcr
                                           ON pc.rank = pcr.rank
                                  WHERE  pcr.rank_order > bcr.rank_order
                                         AND pc.likelihood >
                                             (SELECT val
                                              FROM   params
                                              WHERE  name = 'multiclass_likelihood')
                                  GROUP  BY placement_id,
                                            desired_rank
                                  HAVING COUNT(*) <= (SELECT val
                                                      FROM   params
                                                      WHERE  name = 'multiclass_count')
                                  ORDER  BY pcr.rank_order)
                          GROUP  BY placement_id) USING (placement_id, rank)
               LEFT JOIN placement_classifications pc
                 ON pc.placement_id = bc.placement_id
                    AND pc.desired_rank = pc.rank
                    AND pc.rank = COALESCE(below_rank, bc.rank)
        WHERE  COALESCE(pc.likelihood > (SELECT val
                                         FROM   params
                                         WHERE  name = 'multiclass_likelihood'), 1);

      CREATE VIEW _bayes_base
      AS
        SELECT *
          FROM (SELECT placement_id,
                       rank,
                       evidence
                  FROM placement_evidence pe
                       JOIN ranks USING (rank)
                 WHERE bayes_factor >= (SELECT val
                                          FROM params
                                         WHERE name = 'bayes_cutoff')
                 ORDER BY placement_id,
                          rank_order)
         GROUP BY placement_id;

      CREATE VIEW bayes_multiclass
      AS
        SELECT placement_id,
               rank,
               tax_id,
               likelihood,
               evidence
          FROM _bayes_base
               JOIN placement_classifications pc USING (placement_id, rank)
         WHERE rank = pc.desired_rank;

      CREATE VIEW bayes_best_classifications
      AS
        SELECT placement_id,
               rank,
               tax_id,
               likelihood,
               evidence
          FROM (SELECT *
                  FROM _bayes_base
                       JOIN placement_classifications USING (placement_id, rank)
                 ORDER BY placement_id,
                          likelihood ASC)
         GROUP BY placement_id

    ";
    let st = Sqlite3.prepare db "INSERT INTO params VALUES (?, ?)" in
    List.iter
      (Sql.bind_step_reset db st)
      [
        [| Sql.D.TEXT "likelihood_cutoff"; Sql.D.FLOAT (fv default_cutoff) |];
        [| Sql.D.TEXT "multiclass_count"; Sql.D.INT (fv default_count |> Int64.of_int) |];
        [| Sql.D.TEXT "multiclass_likelihood"; Sql.D.FLOAT (fv default_likelihood) |];
        [| Sql.D.TEXT "bayes_cutoff"; Sql.D.FLOAT (fv default_bayes_cutoff) |];
      ];
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

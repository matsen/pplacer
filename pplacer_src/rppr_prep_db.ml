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
  val default_bayes_cutoff = flag "--default-bayes-cutoff"
    (Formatted (1., "The default value for the bayes_cutoff param. Default: %0.2f"))
  val default_multiclass_min = flag "--default-multiclass-min"
    (Formatted (0.2, "The default value for the multiclass_min param. Default: %0.2f"))
  val default_multiclass_sum = flag "--default-multiclass-sum"
    (Formatted (0.8, "The default value for the multiclass_sum param. Default: %0.2f"))

  method specl =
    super_refpkg#specl
    @ super_sqlite#specl
    @ [
      float_flag default_cutoff;
      float_flag default_bayes_cutoff;
      float_flag default_multiclass_min;
      float_flag default_multiclass_sum;
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

      CREATE TABLE IF NOT EXISTS placement_median_identities (
        placement_id INTEGER REFERENCES placements (placement_id) NOT NULL,
        tax_id TEXT REFERENCES taxa (tax_id) NOT NULL,
        median_percent_identity REAL NOT NULL,
        PRIMARY KEY (placement_id, tax_id)
      );
      CREATE INDEX placement_median_identities_id ON placement_median_identities (placement_id);

      CREATE VIEW best_classifications
      AS
        SELECT *
          FROM (SELECT placement_id,
                       tax_id,
                       r_want.rank AS want_rank,
                       r_got.rank  AS rank,
                       likelihood
                  FROM placement_classifications pc,
                       ranks r_got,
                       ranks r_want
                 WHERE pc.rank = pc.desired_rank
                   AND pc.rank = r_got.rank
                   AND r_got.rank_order <= r_want.rank_order
                   AND likelihood >= (SELECT val
                                       FROM params
                                      WHERE name = 'likelihood_cutoff')
                 ORDER BY placement_id,
                          r_want.rank_order,
                          r_got.rank_order ASC,
                          likelihood ASC)
         GROUP BY placement_id,
                  want_rank;

      CREATE VIEW multiclass
      AS
        SELECT placement_id,
               tax_id,
               want_rank,
               rank,
               likelihood
          FROM (SELECT *
                  FROM (SELECT placement_id,
                               r_got.rank  AS rank,
                               r_want.rank AS want_rank
                          FROM placement_classifications pc,
                               ranks r_got,
                               ranks r_want
                         WHERE pc.rank = pc.desired_rank
                           AND pc.rank = r_got.rank
                           AND r_got.rank_order <= r_want.rank_order
                           AND likelihood >= (SELECT val
                                               FROM params
                                              WHERE name = 'multiclass_min')
                         GROUP BY placement_id,
                                  r_want.rank,
                                  r_got.rank
                        HAVING SUM(pc.likelihood) >= (SELECT val
                                                       FROM params
                                                      WHERE name = 'likelihood_cutoff')
                         ORDER BY r_got.rank_order ASC)
                 GROUP BY placement_id,
                          want_rank)
               JOIN placement_classifications pc USING (placement_id, rank)
         WHERE pc.rank = pc.desired_rank;

      CREATE VIEW placement_evidence_ranks
      AS
        SELECT *
          FROM placement_evidence
               JOIN ranks USING (rank);

      CREATE VIEW _bayes_base
      AS
        SELECT *
          FROM (SELECT per_want.placement_id,
                       per_want.rank AS want_rank,
                       CASE
                         WHEN per_below.rank IS NOT NULL THEN per_want.rank
                         ELSE per_above.rank
                       END           AS rank
                  FROM placement_evidence_ranks per_want
                       LEFT JOIN placement_evidence_ranks per_above
                         ON per_want.placement_id = per_above.placement_id
                            AND per_above.bayes_factor >= (SELECT val
                                                             FROM params
                                                            WHERE name = 'bayes_cutoff')
                            AND per_want.rank_order >= per_above.rank_order
                       LEFT JOIN placement_evidence_ranks per_below
                         ON per_want.placement_id = per_below.placement_id
                            AND per_below.bayes_factor >= (SELECT val
                                                             FROM params
                                                            WHERE name = 'bayes_cutoff')
                            AND per_want.rank_order <= per_below.rank_order
                 ORDER BY per_want.placement_id,
                          per_want.rank_order,
                          per_above.rank_order ASC)
         GROUP BY placement_id,
                  want_rank;

      CREATE VIEW bayes_multiclass
      AS
        SELECT placement_id,
               want_rank,
               rank,
               tax_id,
               likelihood
          FROM _bayes_base
               JOIN placement_classifications pc USING (placement_id, rank)
         WHERE rank = pc.desired_rank;

      CREATE VIEW bayes_best_classifications
      AS
        SELECT placement_id,
               want_rank,
               rank,
               tax_id,
               likelihood
          FROM (SELECT *
                  FROM _bayes_base
                       JOIN placement_classifications USING (placement_id, rank)
                 ORDER BY placement_id,
                          want_rank,
                          likelihood ASC)
         GROUP BY placement_id,
                  want_rank

    ";
    let st = Sqlite3.prepare db "INSERT INTO params VALUES (?, ?)" in
    List.iter
      (Sql.bind_step_reset db st)
      [
        [| Sql.D.TEXT "likelihood_cutoff"; Sql.D.FLOAT (fv default_cutoff) |];
        [| Sql.D.TEXT "bayes_cutoff"; Sql.D.FLOAT (fv default_bayes_cutoff) |];
        [| Sql.D.TEXT "multiclass_min"; Sql.D.FLOAT (fv default_multiclass_min) |];
        [| Sql.D.TEXT "multiclass_sum"; Sql.D.FLOAT (fv default_multiclass_sum) |];
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

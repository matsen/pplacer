#!/usr/bin/env python
"""This script serves two roles:

* Depending on the classification method used, the classification
  database may be left in an inconsistent/incomplete state. In
  particular, the multiclass table may not contain classifications for
  all the sequences needed to obtain accurate read count tallies from
  classif_table. This script fixes these issues, but must have the
  dedup_info data if the sequences you passed to guppy classify were
  deduplicated.

* Creates a view `multiclass_concat` and add names for concatenated
  taxids to the taxa table. This step is also necessary to run
  classif_table, which groups results by these concatenated taxa.

Note that the file specified by `-d/--dedup-info` is the output of
`deduplicate_sequence.py -d/--deduplicated-sequences-file` and is a
headerless csv file with fields (kept_seq_id,orig_seq_id,count). This
file is necessary for obtaining accurate results if the sequences
provided to pplacer were deduplicated, and `guppy classify` was run
with any method other than the "pplacer" method.

"""

import itertools
import argparse
import operator
import warnings
import logging
import sqlite3
import csv

log = logging.getLogger(__name__)
warnings.filterwarnings("always", category=UserWarning)

rerun_warning_message = """
It appears that you already have a table named old_placement_names. Presumably, you have already run
multiclass_concat on this database and opted for --keep-tables. As such, we are leaving old_placement_names
intact instead of renaming placement_names to old_placement_names
"""

no_dedup_info_error = """
You did not to pass in a --dedup-info file! Unless you did not perform deduplication or are using the
'pplacer' classification method, this will lead to inconsistent results in downstream analyses using this
database. If you did not perform deduplication, please specify the --no-dedup flag to avoid this Exception.
"""


def concat_name(taxnames, rank, sep='/'):
    """Heuristics for creating a sensible combination of species names."""
    splits = [x.split() for x in taxnames]

    if (rank == 'species'
            and all(len(x) > 1 for x in splits)
            and len(set(s[0] for s in splits)) == 1):
        name = '%s %s' % (splits[0][0],
                          sep.join(sorted('_'.join(s[1:]) for s in splits)))
    else:
        name = sep.join('_'.join(s) for s in splits)

    return name


def add_multiclass_concat(database):
    """Does all the work of creating a multiclass concat table, populating it, and
    ensuring there are taxa records for the concatenated taxa."""

    curs = database.cursor()
    curs.execute('DROP TABLE IF EXISTS multiclass_concat')

    curs.execute("""
        CREATE TABLE multiclass_concat AS
        SELECT placement_id,
               name,
               want_rank,
               GROUP_CONCAT(DISTINCT tax_id) AS tax_id,
               GROUP_CONCAT(DISTINCT rank)   AS rank,
               SUM(likelihood)               AS likelihood,
               COUNT(*)                      AS id_count
          FROM multiclass
         GROUP BY placement_id,
                  name,
                  want_rank
    """)

    curs.execute('CREATE INDEX multiclass_concat_name ON multiclass_concat (name)')

    # Get all of the constructed tax_ids and their constituent tax_names.
    curs.execute("""
        SELECT DISTINCT mcc.tax_id,
                        mc.rank,
                        t.tax_name
          FROM multiclass_concat mcc
               JOIN multiclass mc USING (placement_id, name, want_rank)
               JOIN taxa t
                 ON t.tax_id = mc.tax_id
         WHERE id_count > 1
    """)

    new_taxa = itertools.groupby(curs, operator.itemgetter(slice(None, 2)))
    def build_new_taxa():
        for (tax_id, rank), names in new_taxa:
            new_name = concat_name([name for _, _, name in names], rank)
            log.info('adding %r as %r at %r', tax_id, new_name, rank)
            yield tax_id, rank, new_name

    # We need another cursor so we can read from one and write using the other.
    database.cursor().executemany(
        "INSERT OR REPLACE INTO taxa (tax_id, rank, tax_name) VALUES (?, ?, ?)",
        build_new_taxa())

    database.commit()


def clean_database(database, dedup_info):
    """This function cleans up the database when deduplication has been used so that 1) the placement_name
    records pointed to in multiclass have the correct mass and 2) multiclass is inflated so that for each set
    of sequences deduplicated to a single sequence, there is at least one sequence (placement_name pointed to)
    represented in multiclass for each specimen."""

    curs = database.cursor()

    # First create an index on multiclass. This will ensure we don't put anything in twice
    curs.execute("""CREATE UNIQUE INDEX IF NOT EXISTS multiclass_index
                        ON multiclass (name, want_rank, tax_id)""")

    try:
        # Rename placement_names so that we can create a new, correct placement_names. If user specifies
        # --keep-tables, we'll leave this copy in, in case they need it for mucking with things
        curs.execute("""ALTER TABLE placement_names RENAME
                           TO old_placement_names""")
    except sqlite3.OperationalError:
        # If we can't do this, this is probably the second time running the script and the user wanted to keep
        # tables, so raise a warning message for them, and just delete placement_names
        warnings.warn(rerun_warning_message)
        curs.execute("DROP TABLE IF EXISTS placement_names")

    # Create the new placement_names table (note origin, which is in the origin, is not needed here)
    curs.execute("""
        CREATE TABLE placement_names (
               placement_id INTEGER NOT NULL,
               name TEXT NOT NULL,
               mass REAL NOT NULL,
               PRIMARY KEY (name))""")

    # Read the dedup info into a new dedup_info table. We need this for it's masses and for inflating
    # multiclass so it has classifications for all of the sequences it's supposed to
    curs.execute("DROP TABLE IF EXISTS dedup_info")
    curs.execute("""
        CREATE TABLE dedup_info (
               global_rep TEXT NOT NULL,
               specimen_rep TEXT NOT NULL,
               mass REAL NOT NULL, PRIMARY KEY (global_rep, specimen_rep))""")
    curs.executemany("INSERT INTO dedup_info VALUES (?, ?, ?)", dedup_info)

    # POPULATING placement_names:
    # First - fill with the things that we have matches for in the multiclass table. We'll use the
    # placement_id values in multiclass as the corresponding values in placement_names and get masses from
    # dedup_info
    curs.execute("""
        INSERT INTO placement_names
        SELECT placement_id,
               name,
               mass
          FROM dedup_info
               JOIN (SELECT DISTINCT placement_id, name
                       FROM multiclass) ON name = specimen_rep""")
    # Next - fill with the things there weren't name matches for in multiclass. Here, we look for the names
    # in dedup_info that are missing in multiclass, then use the global_rep match found in placement_names for
    # a (somewhat dummy) placement_id. WARNING! this placement_id may not actually be consistent with the
    # extraneous tables in the database (those deleted if --keep-tables isn't specified).
    curs.execute("""
        INSERT INTO placement_names
        SELECT placement_id,
               specimen_rep,
               di.mass
          FROM placement_names
               JOIN (SELECT *
                       FROM dedup_info
                      WHERE specimen_rep NOT IN (SELECT DISTINCT name from multiclass)) di
               ON global_rep = name""")

    # Inflate multiclass - now that placement_names is complete, we find the names that are missing in the
    # multiclass table, and using dedup_info, figure out for each of these names what the "global_rep" name is
    # that already lies in multiclass and create a copy of those mc tables where the name and placement id
    # point to the afore mentioned "missing" placement_id and name in placement_names.
    curs.execute("""
        INSERT INTO multiclass
        SELECT pn.placement_id, specimen_rep, want_rank, rank, tax_id, likelihood
          FROM (SELECT *
                  FROM placement_names
                 WHERE name NOT IN (SELECT DISTINCT name from multiclass)) pn
               JOIN dedup_info    ON pn.name = specimen_rep
               JOIN multiclass mc ON mc.name = global_rep""")

    # Commit database
    database.commit()


def drop_uneeded_tables(database):
    """Since the most pertinent tables are already going to be included in the database, we can drop all the
    extra "housekeeping" tables used in the classification process to conserve space and consistency."""
    curs = database.cursor()
    for table in ["placement_classifications",
                    "placement_evidence",
                    "placement_median_identities",
                    "placement_nbc",
                    "placement_positions",
                    "old_placement_names",
                    "dedup_info",
                    "runs"]:
        curs.execute("DROP TABLE IF EXISTS %s" % table)
    database.commit()


def main():
    logging.basicConfig(
        level=logging.INFO, format="%(levelname)s: %(message)s")

    parser = argparse.ArgumentParser(description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('database', type=sqlite3.connect,
        help="sqlite database (output of `rppr prep_db` after `guppy classify`)")
    parser.add_argument('-d', '--dedup-info', type=argparse.FileType('r'),
                        help="""Headerless CSV file with fields
                        "kept_seq_id","orig_seq_id","count" (see note above)""")
    parser.add_argument('-n', '--no-dedup', action="store_true",
        help="""If the classified data was not deduplicated and you can not pass in --dedup-info, this flag
        must be specified to avoid a runtime error.""")
    parser.add_argument('-k', '--keep-tables', action="store_true",
        help="""If specified, keep "scratch work" tables used in classification algorithms, and a copy of the
        current placement_names table renamed to old_placement_names. This option is advised against as
        placement_id consistency is not guaranteed with these tables; they can also take up quite a bit of
        space""")
    args = parser.parse_args()


    # Clean up the database so that masses will come out correctly/completely for all specimens downstream
    if args.dedup_info:
        log.info("Starting cleanup")
        dedup_info_reader = csv.reader(args.dedup_info)
        clean_database(args.database, dedup_info_reader)
    elif not args.no_dedup:
        raise Exception(no_dedup_info_error)

    # Run the actual multiclass_concat code
    log.info("Adding multiclass_concat")
    add_multiclass_concat(args.database)

    if not args.keep_tables:
        log.info("Removing uneeded tables")
        drop_uneeded_tables(args.database)


if __name__ == '__main__':
    main()



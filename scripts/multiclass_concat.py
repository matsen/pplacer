#!/usr/bin/python
"""
In a classification database, create a view `multiclass_concat` and add names
for concatenated taxids to the taxa table.
"""

import itertools
import argparse
import operator
import logging
import sqlite3

def concat_name(taxnames, rank, sep='/'):
    """
    Heuristics for creating a sensible combination of species names.
    """

    splits = [x.split() for x in taxnames]

    if (rank == 'species'
            and all(len(x) > 1 for x in splits)
            and len(set(s[0] for s in splits)) == 1):
        name = '%s %s' % (splits[0][0],
                          sep.join(sorted('_'.join(s[1:]) for s in splits)))
    else:
        name = sep.join('_'.join(s) for s in splits)

    return name

log = logging.getLogger(__name__)

def main():
    logging.basicConfig(
        level=logging.INFO, format="%(levelname)s: %(message)s")

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('database', type=sqlite3.connect,
        help='sqlite database (output of `rppr prep_db` after `guppy classify`)')


    args = parser.parse_args()
    curs = args.database.cursor()

    curs.execute('DROP VIEW IF EXISTS multiclass_concat')

    curs.execute("""
        CREATE VIEW multiclass_concat AS
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
    args.database.cursor().executemany(
        "INSERT OR REPLACE INTO taxa (tax_id, rank, tax_name) VALUES (?, ?, ?)",
        build_new_taxa())

    args.database.commit()

main()

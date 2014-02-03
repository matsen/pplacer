#!/usr/bin/env python
"""
Produce rectangular matrices representing various counts in a classification
database.
"""

import argparse
import logging
import sqlite3
import csv
import warnings

warnings.filterwarnings("always", category=DeprecationWarning)
deprecation_message="""
ATTENTION!!!  This script (classif_rect.py) has been deprecated in favor of the streamlined classif_table.py,
and will be removed entirely in a future release. Please switch over when you are able.
"""
warnings.warn(deprecation_message, DeprecationWarning, 2)


log = logging.getLogger(__name__)

def cursor_to_csv(curs, outfile, description=None):
    if description is None:
        description = curs.description
    writer = csv.writer(outfile)
    writer.writerow([d[0] for d in description])
    writer.writerows(curs)

def by_taxon(args):
    """This one just normalizes (for the normalized_tally) by taxon"""
    # a bit ugly. maybe in the future we'll use sqlalchemy.
    specimen_join = ''
    if args.specimen_map:
        specimen_join = 'JOIN specimens USING (name)'

    log.info('tabulating by_taxon')
    curs = args.database.cursor()
    curs.execute("""
        SELECT COALESCE(tax_name, "unclassified") tax_name,
               COALESCE(t.tax_id, "none")         tax_id,
               COALESCE(t.rank, "root")           rank,
               SUM(mass)                          tally,
               COUNT(DISTINCT placement_id)       placements
          FROM multiclass_concat mc
               JOIN placement_names USING (name, placement_id)
               %s
               LEFT JOIN taxa t USING (tax_id)
         WHERE want_rank = ?
         GROUP BY t.tax_id
         ORDER BY tally DESC
    """ % (specimen_join,), (args.want_rank,))

    with args.by_taxon:
        cursor_to_csv(curs, args.by_taxon)

def by_taxon_normalized_by_specimen(args, by_specimen_results, specimens):
    """This one normalizes first by specimen, using the results from
    by_specimen, then by taxon for the normalized_tally results."""

    taxa_cols = ['tax_id', 'tax_name', 'rank']
    n_specimens = len(specimens)
    results = by_specimen_results.values()

    cols = taxa_cols + ['normalized_tally']
    with args.by_taxon:
        writer = csv.writer(args.by_taxon)
        writer.writerow(cols)
        for result in results:
            row = [result[c] for c in taxa_cols]
            tax_total = sum([result[s] for s in specimens if s in
                result.keys()])
            row.append(tax_total / n_specimens)
            writer.writerow(row)


def by_specimen(args):
    log.info('tabulating total per specimen mass sums')
    curs = args.database.cursor()
    curs.execute("""
        CREATE TEMPORARY TABLE specimen_mass
               (specimen, total_mass, PRIMARY KEY (specimen))""")

    curs.execute("""
        INSERT INTO specimen_mass
        SELECT specimen, SUM(mass)
          FROM specimens
               JOIN multiclass_concat mc USING (name)
               JOIN placement_names USING (name, placement_id)
         WHERE want_rank = ?
         GROUP BY specimen
    """, (args.want_rank,))

    log.info('tabulating counts by specimen')
    curs.execute("""
        SELECT specimen,
               COALESCE(tax_name, "unclassified") tax_name,
               COALESCE(t.tax_id, "none")         tax_id,
               COALESCE(t.rank, "root")           rank,
               SUM(mass)                          tally,
               COUNT(DISTINCT placement_id)       placements,
               SUM(mass) / sm.total_mass          normalized_tally
          FROM specimens
               JOIN specimen_mass sm USING (specimen)
               JOIN multiclass_concat mc USING (name)
               JOIN placement_names USING (name, placement_id)
               LEFT JOIN taxa t USING (tax_id)
         WHERE want_rank = ?
         GROUP BY specimen, t.tax_id
         ORDER BY tally DESC
    """, (args.want_rank,))

    desc = curs.description
    rows = curs.fetchall()
    if args.group_by_specimen:
        log.info('writing group_by_specimen')
        with args.group_by_specimen:
            cursor_to_csv(rows, args.group_by_specimen, desc)

    results = {}
    specimens = set()
    for specimen, tax_name, tax_id, rank, tally, placements, normalized_tally in rows:
        row = results.get(tax_id)
        if row is None:
            row = results[tax_id] = dict(tax_id=tax_id, tax_name=tax_name, rank=rank)
        row[specimen] = normalized_tally if args.normalize_by_specimen else tally
        specimens.add(specimen)

    log.info('writing by_specimen')
    cols = ['tax_name', 'tax_id', 'rank'] + list(specimens)
    with args.by_specimen:
        writer = csv.DictWriter(args.by_specimen, cols, restval=0)
        writer.writerow(dict(zip(cols, cols)))  # ugh, DictWriter :(

        if args.metadata_map:
            for key in {k for data in args.metadata.itervalues() for k in data}:
                d = {specimen: args.metadata.get(specimen, {}).get(key)
                     for specimen in specimens}
                d['tax_name'] = key
                d['tax_id'] = d['rank'] = ''
                writer.writerow(d)

        writer.writerows(results.itervalues())

    return (results, specimens)


def main():
    logging.basicConfig(
        level=logging.INFO, format="%(asctime)s %(levelname)s: %(message)s")

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('database', type=sqlite3.connect,
        help='sqlite database (output of `rppr prep_db` after `guppy classify`)')
    parser.add_argument('by_taxon', type=argparse.FileType('w'),
        help='output CSV file which counts results by taxon')
    parser.add_argument('by_specimen', type=argparse.FileType('w'), nargs='?',
        help='optional output CSV file which counts results by specimen (requires specimen map)')
    parser.add_argument('group_by_specimen', type=argparse.FileType('w'), nargs='?',
        help='optional output CSV file which groups results by specimen (requires specimen map)')
    parser.add_argument('-r', '--want-rank', default='species', metavar='RANK',
        help='want_rank at which to tabulate results (default: %(default)s)')
    parser.add_argument('-m', '--specimen-map', type=argparse.FileType('r'), metavar='CSV',
        help='input CSV map from sequences to specimens')
    parser.add_argument('--metadata-map', type=argparse.FileType('r'), metavar='CSV',
        help='input CSV map including a specimen column and other metadata')
    parser.add_argument('--normalize-by-specimen', default=False, action='store_true',
        help="""By taxon output has normalized_tally results be normalized by
        specimen first, then by taxon. Additionally, the output of by-specimen
        will use normalized_tally rather than tally.""")

    args = parser.parse_args()
    if args.by_specimen and not args.specimen_map:
        parser.error('specimen map is required for by-specimen output')
    if args.normalize_by_specimen and not args.by_specimen:
        parser.error('must compute by-specimen in order to normalize by specimen')

    if args.specimen_map:
        log.info('populating specimens table from specimen map')
        curs = args.database.cursor()
        curs.execute("CREATE TEMPORARY TABLE specimens (name, specimen, PRIMARY KEY (name, specimen))")
        with args.specimen_map:
            reader = csv.reader(args.specimen_map)
            curs.executemany("INSERT INTO specimens VALUES (?, ?)", reader)

    if args.metadata_map:
        log.info('reading metadata map')
        with args.metadata_map:
            reader = csv.DictReader(args.metadata_map)
            args.metadata = {data['specimen']: data for data in reader}

    if args.by_specimen:
        by_specimen_results, specimens = by_specimen(args)
    if args.normalize_by_specimen:
        by_taxon_normalized_by_specimen(args, by_specimen_results, specimens)
    else:
        by_taxon(args)


main()



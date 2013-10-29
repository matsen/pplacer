#!/usr/bin/env python
"""
Produce csv tables representing various counts in a classifications database.

The output options available depend on whether a specimen map is specified.
Without a specimen map, only by_taxon output is available. If a specimen map
is provided, two further output formats are possible: by_specimen_long and
by_specimen_wide.

* by_specimen_long - Contains a separate row for each specimen taxa combination
* by_specimen_wide - Rows are taxa, columns are specimens

Collectively, three counts are found in these output files: tally, placements,
frequency and average_frequency.

* placements - Number of placements classified as a given taxa, not corrected
    for placement/read duplicity.
* tally - Placements count adjusted to reflect the total number of reads.
* frequency - In by_specimen output, the frequency of reads matching a given
    taxa for some specimen.
* frequency_average - In by_taxon output, the average frequency of the taxa
    across specimens (optional).
"""

import argparse
import logging
import sqlite3
import csv

log = logging.getLogger(__name__)

def cursor_to_csv(curs, outfile, description=None):
    if description is None:
        description = curs.description
    writer = csv.writer(outfile)
    writer.writerow([d[0] for d in description])
    writer.writerows(curs)

def by_taxon(args):
    """This function queries for the tally and placement count totals for each taxon at the desired want_rank.
    If a specimen map is specified, it only returns this data for those specimen/sequences in the map."""
    # a bit ugly. maybe in the future we'll use sqlalchemy.
    specimen_join = 'JOIN specimens USING (name)' if args.specimen_map else ''

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

def by_taxon_average_frequencies(args, by_specimen_results, specimens):
    """This function uses the by_specimen results to compute, for each taxon, the average frequency of the that
    taxon across specimens."""

    log.info('tabulating by_taxon using by_specimen results for frequency')
    taxa_cols = ['tax_id', 'tax_name', 'rank']
    n_specimens = len(specimens)
    results = by_specimen_results.values()

    cols = taxa_cols + ['average_frequency']
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
               SUM(mass) / sm.total_mass          frequency
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
    if args.by_specimen_tall:
        # By specimen tall
        log.info('writing by_specimen_tall')
        with args.by_specimen_tall:
            cursor_to_csv(rows, args.by_specimen_tall, desc)

    results = {}
    specimens = set()
    for specimen, tax_name, tax_id, rank, tally, placements, frequency in rows:
        row = results.get(tax_id)
        if row is None:
            row = results[tax_id] = dict(tax_id=tax_id, tax_name=tax_name, rank=rank)
        row[specimen] = frequency if args.frequencies else tally
        specimens.add(specimen)

    # By specimen wide
    log.info('writing by_specimen_wide')
    cols = ['tax_name', 'tax_id', 'rank'] + list(specimens)
    with args.by_specimen_wide:
        writer = csv.DictWriter(args.by_specimen_wide, fieldnames=cols, restval=0)
        writer.writeheader()

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

    parser = argparse.ArgumentParser(description=__doc__,
            formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('database', type=sqlite3.connect,
        help='sqlite database (output of `rppr prep_db` after `guppy classify`)')
    parser.add_argument('by_taxon', type=argparse.FileType('w'),
        help='output CSV file which counts results by taxon')
    parser.add_argument('by_specimen_wide', type=argparse.FileType('w'), nargs='?',
        help='optional output CSV file with count results by specimen and taxa (requires specimen map)')
    parser.add_argument('by_specimen_tall', type=argparse.FileType('w'), nargs='?',
        help='optional output CSV file with rows as taxa and columns as specimens (requires specimen map)')
    parser.add_argument('-r', '--want-rank', default='species', metavar='RANK',
        help='want_rank at which results are to be tabulated (default: %(default)s)')
    parser.add_argument('-m', '--specimen-map', type=argparse.FileType('r'), metavar='CSV',
        help='input CSV map from sequences to specimens')
    parser.add_argument('-M', '--metadata-map', type=argparse.FileType('r'), metavar='CSV',
        help="""input CSV map including a specimen column and other metadata; if specified gets merged in with
        by specimen output.""")
    parser.add_argument('-f', '--frequencies', default=False, action='store_true',
        help="""If specified, by_taxon output has an average_frequency column instead of tally and
        placements columns, and by_specimen_tall output uses frequency intead of tally.""")

    args = parser.parse_args()
    if args.by_specimen_wide and not args.specimen_map:
        parser.error('specimen map is required for by_specimen output')
    if args.frequencies and not args.by_specimen_wide:
        parser.error('must compute by-specimen in order to compute frequencies')

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

    if args.by_specimen_wide:
        by_specimen_results, specimens = by_specimen(args)
    if args.frequencies:
        by_taxon_average_frequencies(args, by_specimen_results, specimens)
    else:
        by_taxon(args)


main()


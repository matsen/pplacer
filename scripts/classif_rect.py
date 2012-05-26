#!/usr/bin/python
"""
Produce rectangular matrices representing various counts in a classification
database.
"""

import argparse
import logging
import sqlite3
import csv

log = logging.getLogger(__name__)

def cursor_to_csv(curs, outfile):
    writer = csv.writer(outfile)
    writer.writerow([d[0] for d in curs.description])
    writer.writerows(curs)

def by_taxon(args):
    curs = args.database.cursor()
    curs.execute("""
        SELECT COALESCE(tax_name, "unclassified") tax_name,
               COALESCE(tax_id, "none")           tax_id,
               COALESCE(t.rank, "root")           rank,
               COUNT(*)                           tally
          FROM multiclass_concat
               LEFT JOIN taxa t USING (tax_id)
         WHERE want_rank = ?
         GROUP BY tax_id
         ORDER BY tally DESC
    """, (args.want_rank,))

    with args.by_taxon:
        cursor_to_csv(curs, args.by_taxon)

def by_specimen(args):
    curs = args.database.cursor()
    curs.execute("CREATE TEMPORARY TABLE specimens (name, specimen)")
    with args.specimen_map:
        reader = csv.reader(args.specimen_map)
        curs.executemany("INSERT INTO specimens VALUES (?, ?)", reader)

    curs.execute("""
        SELECT specimen,
               COALESCE(tax_name, "unclassified") tax_name,
               COALESCE(tax_id, "none")           tax_id,
               COALESCE(t.rank, "root")           rank,
               COUNT(*)                           tally
          FROM specimens
               JOIN multiclass_concat USING (name)
               LEFT JOIN taxa t USING (tax_id)
         WHERE want_rank = ?
         GROUP BY specimen, tax_id
         ORDER BY tally DESC
    """, (args.want_rank,))

    results = {}
    specimens = set()
    for specimen, tax_name, tax_id, rank, tally in curs:
        row = results.get(tax_id)
        if row is None:
            row = results[tax_id] = dict(tax_id=tax_id, tax_name=tax_name, rank=rank)
        row[specimen] = tally
        specimens.add(specimen)

    cols = ['tax_name', 'tax_id', 'rank'] + list(specimens)
    with args.by_specimen:
        writer = csv.DictWriter(args.by_specimen, cols, restval=0)
        writer.writerow(dict(zip(cols, cols)))  # ugh, DictWriter :(
        writer.writerows(results.itervalues())


def main():
    logging.basicConfig(
        level=logging.INFO, format="%(levelname)s: %(message)s")

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('database', type=sqlite3.connect,
        help='sqlite database (output of `rppr prep_db` after `guppy classify`)')
    parser.add_argument('by_taxon', type=argparse.FileType('w'),
        help='output CSV file which counts results by taxon')
    parser.add_argument('by_specimen', type=argparse.FileType('w'), nargs='?',
        help='optional output CSV file which counts results by specimen (requires specimen map)')
    parser.add_argument('-r', '--want-rank', default='species', metavar='RANK',
        help='want_rank at which to tabulate results (default: %(default)s)')
    parser.add_argument('-m', '--specimen-map', type=argparse.FileType('r'), metavar='CSV',
        help='input CSV map from sequences to specimens')

    args = parser.parse_args()
    if args.by_specimen and not args.specimen_map:
        parser.error('specimen map is required for by-specimen output')

    by_taxon(args)
    if args.by_specimen:
        by_specimen(args)

main()


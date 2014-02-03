#!/usr/bin/env python
"""
Produce csv tables representing various counts in a classification database.

Without a specimen map, only by_taxon output is available. If a specimen map is
provided, additional output options are available presenting results by specimen.
This map also adds columns to by_taxon output (notably the avg_freq column,
indicating average frequency of the corresponding taxon across specimens).
"""

import argparse
import logging
import sqlite3
import csv

log = logging.getLogger(__name__)


# Used various places
taxa_cols = ['tax_id', 'tax_name', 'rank']


def cursor_to_csv(curs, outfile, description=None):
    "Iterate over SQL cursor and write to outfile"
    if description is None:
        description = curs.description
    writer = csv.writer(outfile)
    writer.writerow([d[0] for d in description])
    writer.writerows(curs)


def by_taxon(args):
    """This function queries for the tally and placement count totals for each taxon at the desired rank.
    It does not require a specimen map."""

    log.info('tabulating by_taxon')
    curs = args.database.cursor()
    curs.execute("""
        SELECT COALESCE(tax_name, "unclassified") tax_name,
               COALESCE(t.tax_id, "none")         tax_id,
               COALESCE(t.rank, "root")           rank,
               CAST(SUM(mass) AS INT)             tally, 
               COUNT(DISTINCT placement_id)       placements
          FROM multiclass_concat mc
               JOIN placement_names USING (name, placement_id)
               LEFT JOIN taxa t USING (tax_id)
         WHERE want_rank = ?
         GROUP BY t.tax_id
         ORDER BY tally DESC
    """, (args.rank,))

    with args.by_taxon:
        cursor_to_csv(curs, args.by_taxon)


def by_taxon_with_specimens(args, by_specimen_results, specimens):
    """This function uses by_specimen results to compute, for each taxon, the average frequency of that
    taxon across specimens."""

    log.info('tabulating by_taxon using by_specimen results')
    n_specimens = float(len(specimens))

    cols = taxa_cols + ['tally', 'placements', 'avg_tally', 'avg_placements', 'avg_freq' ]
    with args.by_taxon:
        writer = csv.writer(args.by_taxon)
        writer.writerow(cols)
        # Iterate over taxa in the by_specimen_results
        for result in by_specimen_results:
            row = [result[c] for c in taxa_cols]
            tally, placements, freqs = 0, 0, 0
            # For each specimen, fetch counts from the dict they map to and add to totals
            for spec in specimens:
                if spec in result.keys():
                    spec_result = result[spec]
                    tally += spec_result['tally']
                    placements += spec_result['placements']
                    freqs += spec_result['freq']
            # Add count data to output row, and write out
            row += [tally, placements, tally/n_specimens, placements/n_specimens, freqs/n_specimens]
            writer.writerow(row)


def by_specimen(args):
    """This function computes results by specimen, and if --by-specimen is supplied as an arg, writes results
    out to the specified file. The results of this function are also used downstream in calls to by_taxon_wide
    and by_specimen_wide.
    
    This function returns a list of dicts, and a list of specimens. Each dict has tax_id, tax_name and rank
    key/value pairs. For each specimen, these dicts map the corresponding specimen name to count results for
    that specimen."""

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
    """, (args.rank,))

    log.info('tabulating counts by specimen')
    curs.execute("""
        SELECT specimen,
               COALESCE(tax_name, "unclassified") tax_name,
               COALESCE(t.tax_id, "none")         tax_id,
               COALESCE(t.rank, "root")           rank,
               CAST(SUM(mass) AS INT)             tally,
               COUNT(DISTINCT placement_id)       placements,
               SUM(mass) / sm.total_mass          freq
          FROM specimens
               JOIN specimen_mass sm USING (specimen)
               JOIN multiclass_concat mc USING (name)
               JOIN placement_names USING (name, placement_id)
               LEFT JOIN taxa t USING (tax_id)
         WHERE want_rank = ?
         GROUP BY specimen, t.tax_id
         ORDER BY freq DESC
    """, (args.rank,))

    desc = curs.description
    rows = curs.fetchall()
    # By specimen (tall)
    if args.by_specimen:
        log.info('writing by_specimen (tall)')
        with args.by_specimen:
            cursor_to_csv(rows, args.by_specimen, desc)

    # This will be a dictionary mapping each tax_id to a dictionary of data for the corresponding taxon
    results = {}
    specimens = set()
    for specimen, tax_name, tax_id, rank, tally, placements, freq in rows:
        row = results.get(tax_id)
        if row is None:
            # The taxon dict will include name and rank...
            row = results[tax_id] = dict(tax_id=tax_id, tax_name=tax_name, rank=rank)
        # and for each specimen with classifications for this taxon, a dictionary of the count results
        row[specimen] = dict(tally=tally, placements=placements, freq=freq)
        specimens.add(specimen)

    assert len(specimens.intersection(taxa_cols)) == 0, "The following are invalid specimen names: %r" % taxa_cols
    return (results.values(), specimens)


def by_specimen_wide(args, handle, by_specimen_results, specimens, count):
    """Write out the specified count (tally/freq) into a wide table where rows are taxa and cols are
    specimens"""
    if not count in ('tally', 'freq', 'placements'):
        raise ValueError("Invalid count specified for by_specimen_wide output: {0}".format(count))

    log.info('writing wide output with {0}'.format(count))
    cols = ['tax_name', 'tax_id', 'rank'] + list(specimens)
    with handle:
        writer = csv.DictWriter(handle, fieldnames=cols, restval=0)
        writer.writeheader()

        # Write in the metadata
        if args.metadata_map:
            for key in {k for data in args.metadata.itervalues() for k in data}:
                d = {specimen: args.metadata.get(specimen, {}).get(key)
                     for specimen in specimens}
                d['tax_name'] = key
                d['tax_id'] = d['rank'] = ''
                writer.writerow(d)

        # Little helper for us to safely pass through the count and non-count information from each result in
        # by_specimen_results.
        def count_or_taxdata(v):
            try:
                return v[count]
            except TypeError:
                return v

        # Write in the specified counts
        for row in by_specimen_results:
            row_dict = dict((col, count_or_taxdata(v)) for col, v in row.iteritems())
            writer.writerow(row_dict)


def main():
    logging.basicConfig(
        level=logging.INFO, format="%(asctime)s %(levelname)s: %(message)s")

    # INPUTS
    parser = argparse.ArgumentParser(description=__doc__,
            formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('database', type=sqlite3.connect,
        help='sqlite database (output of `rppr prep_db` after `guppy classify`)')
    parser.add_argument('-m', '--specimen-map', type=argparse.FileType('r'), metavar='CSV',
        help='input CSV map from sequences to specimens (headerless; 1st col sequence id, 2nd specimen name')
    parser.add_argument('-M', '--metadata-map', type=argparse.FileType('r'), metavar='CSV',
        help="""input CSV map including a specimen column and other metadata; if specified gets merged in with
        whichever of freqs_wide and tallies_wide outputs are specified (must include header)""")
    parser.add_argument('-r', '--rank', default='species', metavar='RANK',
        help='rank at which results are to be tabulated (default: %(default)s)')

    # OUTPUTS
    parser.add_argument('by_taxon', type=argparse.FileType('w'),
        help='output CSV file with counts by taxon')
    parser.add_argument('-s', '--by-specimen', type=argparse.FileType('w'), nargs='?',
        help='optional output CSV file with counts by specimen and taxa (requires specimen map)')
    parser.add_argument('-f', '--freqs-wide', type=argparse.FileType('w'), nargs='?',
        help="""optional output CSV file where rows are taxa, columns are specimens, and entries are
        frequencies (requires specimen map)""")
    parser.add_argument('-t', '--tallies-wide', type=argparse.FileType('w'), nargs='?',
        help="""optional output CSV file where rows are taxa, columns are specimens, and entries are
        tallies (requires specimen map)""")

    args = parser.parse_args()
    if (args.by_specimen or args.freqs_wide or args.tallies_wide) and not args.specimen_map:
        parser.error('specimen map is required for by_specimen, freqs_wide and tallies_wide output')

    if args.metadata_map:
        log.info('reading metadata map')
        with args.metadata_map:
            reader = csv.DictReader(args.metadata_map)
            args.metadata = {data['specimen']: data for data in reader}

    if args.specimen_map:
        log.info('populating specimens table from specimen map')
        curs = args.database.cursor()
        curs.execute("CREATE TEMPORARY TABLE specimens (name, specimen, PRIMARY KEY (name, specimen))")
        with args.specimen_map:
            reader = csv.reader(args.specimen_map)
            curs.executemany("INSERT INTO specimens VALUES (?, ?)", reader)
        by_specimen_results, specimens = by_specimen(args)

        # By specimen wide output
        if args.freqs_wide:
            by_specimen_wide(args, args.freqs_wide, by_specimen_results, specimens, "freq")
        if args.tallies_wide:
            by_specimen_wide(args, args.tallies_wide, by_specimen_results, specimens, "tally")
        # By taxon computed from by_specimen results
        by_taxon_with_specimens(args, by_specimen_results, specimens)
        
    else:
        by_taxon(args)


if __name__ == "__main__":
    main()


#!/usr/bin/env python
import collections
import itertools
import argparse
import logging
import sqlite3
import csv
import sys

from taxtastic.taxtable import TaxNode
from taxtastic.refpkg import Refpkg

log = logging.getLogger(__name__)

def group_by_name_and_mass(rows):
    return itertools.groupby(rows, lambda row: (row['seqname'], int(row.get('mass', 1))))

def main():
    logging.basicConfig(
        level=logging.INFO, format="%(levelname)s: %(message)s")

    parser = argparse.ArgumentParser(
        description="Add classifications to a database.")
    parser.add_argument('refpkg', type=Refpkg,
                        help="refpkg containing input taxa")
    parser.add_argument('classification_db', type=sqlite3.connect,
                        help="output sqlite database")
    parser.add_argument('classifications', type=argparse.FileType('r'), nargs='?',
                        default=sys.stdin, help="input query sequences")

    args = parser.parse_args()

    log.info('loading taxonomy')
    taxtable = TaxNode.from_taxtable(args.refpkg.open_resource('taxonomy', 'rU'))
    rank_order = {rank: e for e, rank in enumerate(taxtable.ranks)}
    def full_lineage(node):
        rank_iter = reversed(taxtable.ranks)
        for n in reversed(node.lineage()):
            n_order = rank_order[n.rank]
            yield n, list(itertools.takewhile(lambda r: rank_order[r] >= n_order, rank_iter))

    def multiclass_rows(placement_id, seq, taxa):
        ret = collections.defaultdict(float)
        likelihood = 1. / len(taxa)
        for taxon in taxa:
            for node, want_ranks in full_lineage(taxtable.get_node(taxon)):
                for want_rank in want_ranks:
                    ret[placement_id, seq, want_rank, node.rank, node.tax_id] += likelihood
        for k, v in sorted(ret.items()):
            yield k + (v,)

    curs = args.classification_db.cursor()
    curs.execute('INSERT INTO runs (params) VALUES (?)', (' '.join(sys.argv),))
    run_id = curs.lastrowid

    log.info('inserting classifications')
    for (name, mass), rows in group_by_name_and_mass(csv.DictReader(args.classifications)):
        curs.execute('INSERT INTO placements (classifier, run_id) VALUES ("csv", ?)', (run_id,))
        placement_id = curs.lastrowid
        curs.execute(
            'INSERT INTO placement_names (placement_id, name, origin, mass) VALUES (?, ?, ?, ?)',
            (placement_id, name, args.classifications.name, mass))
        taxa = [row['tax_id'] for row in rows]
        curs.executemany('INSERT INTO multiclass VALUES (?, ?, ?, ?, ?, ?)',
                         multiclass_rows(placement_id, name, taxa))

    log.info('cleaning up `multiclass` table')
    curs.execute("""
        CREATE TEMPORARY TABLE duplicates AS
          SELECT name
            FROM multiclass
                 JOIN placements USING (placement_id)
           GROUP BY name
          HAVING SUM(run_id = ?)
                 AND COUNT(DISTINCT run_id) > 1
    """, (run_id,))
    curs.execute("""
        DELETE FROM multiclass
         WHERE (SELECT run_id
                  FROM placements p
                 WHERE p.placement_id = multiclass.placement_id) <> ?
           AND name IN (SELECT name
                          FROM duplicates)
    """, (run_id,))

    args.classification_db.commit()

main()

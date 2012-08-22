#!/usr/bin/env python
import argparse
import logging
import json
import csv

#from taxtastic.taxtable import TaxNode

log = logging.getLogger(__name__)

ranks = dict(k='kingdom', p='phylum', c='class', o='order', f='family')
rank_order = ['root', 'kingdom', 'phylum', 'class', 'order', 'family']

def main():
    logging.basicConfig(level=logging.INFO,
            format="%(levelname)s: %(message)s")

    parser = argparse.ArgumentParser(
        description="Turn a BIOM file with a taxonomy into a taxtable and seqinfo.")

    parser.add_argument(
        'biom', type=argparse.FileType('r'), help='input BIOM file')
    parser.add_argument(
        'taxtable', type=argparse.FileType('w'), help='output taxtable')
    parser.add_argument(
        'seqinfo', type=argparse.FileType('w'), help='output seqinfo')

    args = parser.parse_args()

    log.info('loading biom')
    with args.biom:
        j = json.load(args.biom)

    root = TaxNode('root', 'Root', name='Root')
    root.ranks = rank_order
    seqinfo = csv.writer(args.seqinfo)
    seqinfo.writerow(('seqname', 'tax_id'))

    log.info('determining tax_ids')
    for leaf in j['rows']:
        taxonomy = leaf['metadata']['taxonomy']
        seqinfo.writerow((leaf['id'], taxonomy[-1]))
        for parent, tax_id in zip(taxonomy, taxonomy[1:]):
            if tax_id in root.index:
                continue
            root.get_node(parent).add_child(
                TaxNode(ranks[tax_id[0]], tax_id, name=tax_id[3:] or tax_id))

    log.info('writing taxtable')
    with args.taxtable:
        root.write_taxtable(args.taxtable)

if __name__ == '__main__':
    main()

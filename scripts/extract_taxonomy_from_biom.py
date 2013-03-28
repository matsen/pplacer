#!/usr/bin/env python
import argparse
import logging
import json
import csv

from taxtastic.taxtable import TaxNode

log = logging.getLogger(__name__)

ranks = dict(k='kingdom', p='phylum', c='class', o='order', f='family',
             g='genus', s='species')
rank_order = ['root', 'kingdom', 'phylum', 'class', 'order', 'family',
              'genus', 'species']

root_id = 'Root'


def lineages(taxonomy, root_name=root_id):
    """
    Yields (<;-delimited lineage>, <node>, <;-delimited parent lineage>)
    tuple for every entry in ``taxonomy``
    """
    parent = root_name
    cur_lineage = [root_name]
    for node in taxonomy:
        cur_lineage.append(node)
        tax_id = ';'.join(cur_lineage)
        yield tax_id, node, parent
        parent = tax_id


def main():
    logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")

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

    root = TaxNode('root', root_id, name='Root')
    root.ranks = rank_order
    seqinfo = csv.writer(args.seqinfo)
    seqinfo.writerow(('seqname', 'tax_id'))

    log.info('determining tax_ids')
    for leaf in j['rows']:
        leaf_taxonomy = leaf['metadata']['taxonomy']

        # Drop nodes containing only rank (e.g. `s__`)
        leaf_taxonomy = [i for i in leaf_taxonomy if i[3:]]
        leaf_lineages = list(lineages([i for i in leaf_taxonomy if i[3:]]))

        seqinfo.writerow((leaf['id'], leaf_lineages[-1][0]))

        for tax_id, node, parent in leaf_lineages:
            if tax_id in root.index:
                continue
            root.get_node(parent).add_child(
                TaxNode(ranks[node[0]], tax_id, name=node[3:] or node))

    log.info('writing taxtable')
    with args.taxtable:
        root.write_taxtable(args.taxtable)


if __name__ == '__main__':
    main()

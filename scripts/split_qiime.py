#!/usr/bin/env python
import argparse
import logging
import csv
import sys
import re

from Bio import SeqIO

log = logging.getLogger(__name__)

description_regexp = re.compile(r'([^\s_]+)_\d+\s+(\S+)')

def main():
    logging.basicConfig(
        level=logging.INFO, format="%(levelname)s: %(message)s")

    parser = argparse.ArgumentParser(
        description="Extract the original sequence names from a QIIME FASTA file.")
    parser.add_argument(
        'qiime', type=argparse.FileType('r'), nargs='?', default=sys.stdin,
        help="input QIIME file (default: stdin)")
    parser.add_argument(
        'fasta', type=argparse.FileType('w'), nargs='?', default=sys.stdout,
        help="output FASTA file (default: stdout)")
    parser.add_argument(
        'specimen_map', type=argparse.FileType('w'), nargs='?',
        help="if specified, output specimen map (default: don't write)")

    args = parser.parse_args()

    specimen_writer = None
    if args.specimen_map is not None:
        specimen_writer = csv.writer(args.specimen_map)

    for seq in SeqIO.parse(args.qiime, 'fasta'):
        match = description_regexp.match(seq.description)
        if not match:
            log.warning("sequence %r had a strange description", seq.id)
            continue
        specimen, seq.id = match.groups()
        seq.description = ''
        SeqIO.write([seq], args.fasta, 'fasta')
        if specimen_writer:
            specimen_writer.writerow((seq.id, specimen))

main()

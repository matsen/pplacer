#!/usr/bin/env python
"""
Deduplicate sequences by group
"""

import argparse
import bz2
import collections
import csv
import gzip
import hashlib
import sys
import textwrap

def sha1(s):
    return hashlib.sha1(s).digest()

class ConstantDict(object):
    """
    Dictionary-like object which returns return_value at every lookup
    """
    def __init__(self, return_value=None):
        self.return_value = return_value

    def __getitem__(self, key):
        return self.return_value

class IdentityDict(object):
    """
    Dictionary-like object which returns the key at every lookup
    """
    def __getitem__(self, key):
        return key

def parse_fasta(fp):
    """
    Parse a FASTA file, yielding (seqid, sequence) tuples

    Based on https://github.com/lh3/readfq
    """
    fp = (i.rstrip() for i in fp)
    last = None
    while True:
        if not last: # the first record or a record following a fastq
            for l in fp: # search for the start of the next record
                if l[0] == '>': # Header
                    last = l
                    break
        if not last:
            break
        name, seqs, last = last[1:].split()[0], [], None
        for l in fp: # read the sequence
            if l[0] == '>':
                last = l
                break
            seqs.append(l)
        yield name, ''.join(seqs)

def write_fasta(records, fp):
    """
    Write a FASTA file from (id, sequence) tuples
    """
    for seq_id, sequence in records:
        fp.write('>{0}\n{1}\n'.format(seq_id, sequence))

class DeduplicatedSequence(object):
    """
    Sequence seen one or more times, with maps to the splits and counts within each split
    """
    __slots__ = ('id', 'sequence', 'sequence_ids', 'split_map', 'counts')

    def __init__(self, seq_id, sequence):
        self.id = seq_id
        self.sequence = sequence
        # Sequence IDs for dedup file
        self.sequence_ids = []
        # Map from sample -> representative sequence ID
        self.split_map = dict()
        # Counts per sequence ID
        self.counts = collections.Counter()

    def add(self, sequence_id, split=None, count=1):
        """
        Add a sequence, with associated split and frequency.

        If another sequence from the same split has been seen previously,
        the count for the original sequence is increased by count. Otherwise,
        sequence_id will be present in the output.
        """
        if split not in self.split_map:
            self.split_map[split] = sequence_id
            self.sequence_ids.append(sequence_id)
        key = self.split_map[split]
        self.counts[key] += count

    def count_records(self):
        """
        Iterator of kept_id, orig_id, frequency tuples
        """
        for sequence_id in self.sequence_ids:
            count = self.counts[sequence_id]
            yield self.id, sequence_id, count

def parse_counts(fp):
    """
    Parse a count file, with sequence ID in first field, count in the second

    Returns a dictionary mapping sequence ID to count.
    """
    reader = csv.reader(fp)
    return {line[0]: int(line[1]) for line in reader}

def parse_splits(fp):
    """
    Parse a split file.

    Returns a dictionary mapping sequence ID to split
    """
    reader = csv.reader(fp)
    return {r[0]: r[1] for r in reader}

def deduplicate_sequences(seqrecords, counts, split_map):
    """
    Deduplicate based on sequence. Returns a list of DeduplicatedSequence
    objects.
    """
    r = {}
    for seq_id, sequence in seqrecords:
        guid = sha1(sequence)
        if guid not in r:
            r[guid] = DeduplicatedSequence(seq_id, sequence)
        r[guid].add(seq_id, split_map[seq_id], counts[seq_id])

    return r.values()

def write_map(deduplicated_sequences, fp):
    """
    Writes (kept_seq_id, orig_seq_id, count) lines
    """
    rows = (row for s in deduplicated_sequences
            for row in s.count_records())
    writer = csv.writer(fp, quoting=csv.QUOTE_MINIMAL, lineterminator='\n')
    writer.writerows(rows)


class Opener(object):
    """Like `argparse.FileType`, but supports gzip and bzip2 file
    compression by inferring compression type from file name suffix.

    Example:

      parser.add_argument('input_file', type=Opener('r'))
      parser.add_argument('output_file', type=Opener('w'))

    """

    def __init__(self, mode = 'r', bufsize = -1):
        self._mode = mode
        self._bufsize = bufsize

    def __call__(self, fname):
        if fname is sys.stdout or fname is sys.stdin:
            return fname
        elif fname == '-':
            return sys.stdin if 'r' in self._mode else sys.stdout
        elif fname.endswith('.bz2'):
            return bz2.BZ2File(fname, self._mode, self._bufsize)
        elif fname.endswith('.gz'):
            mode = ''.join(c for c in self._mode if c not in 'U')
            return gzip.open(fname, mode, self._bufsize)
        else:
            return open(fname, self._mode, self._bufsize)

    def __repr__(self):
        args = self._mode, self._bufsize
        args_str = ', '.join(repr(arg) for arg in args if arg != -1)
        return '{}({})'.format(type(self).__name__, args_str)


def main():
    parser = argparse.ArgumentParser(description=textwrap.dedent("""
    Deduplicate sequences in `input_fasta`, writing the results to
    `output_fasta`, which may then be used with pplacer. A count file suitable
    for use with `guppy redup -m` is written to stdout, by default.

    Note that if you wish to use the `split' placefiles (e.g., split by sample),
    from sequence_id to split_id should be given with `--split-map`.

    Similarly, if your sequences are already deduplicated in some way, you may
    pass in abundance information with `--input-count-file`.

    Input files and output files ending with ".gz" or ".bz2" will be
    opened using gzip or bzip2 file compression, respectively. """),
    formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('-c', '--input-count-file', dest='count_file',
            type=Opener('r'), help="""CSV file with rows containing
            sequence_id (column 1), abundance (column 2) count. If not
            specified, all sequences are given a count of 1.""")
    split_group = parser.add_mutually_exclusive_group()
    split_group.add_argument('-s', '--split-map', type=Opener('r'),
            help="""Headerless CSV file mapping from sequence id (column 1) to
            sample identifier (column 2). If not specified, all sequences are
            assumed to be from a single sample, and are fully deduplicated.""")
    split_group.add_argument('--keep-ids', action='store_true', help="""When
            specified, all sequence IDs will be written to the output file,
            rather than the first ID seen and count""")
    parser.add_argument('input_fasta', type=Opener('r'),
            help="""FASTA file to deduplicate""")
    parser.add_argument('output_fasta', type=Opener('w'),
            help="""Path to write deduplicated FASTA file""")
    parser.add_argument('-d', '--deduplicated-sequences-file',
            type=Opener('w'),
            dest='map_out', default=sys.stdout, help="""Destination for csv
            file of (kept_seq_id,orig_seq_id,count) rows [default: stdout]""")

    a = parser.parse_args()

    if a.count_file:
        with a.count_file as fp:
            counts = parse_counts(fp)
    else:
        # Default to an abundance of 1 for each sequence
        counts = ConstantDict(1)

    # Handle split placefiles
    if a.split_map:
        with a.split_map as fp:
            split_map = parse_splits(fp)
    elif a.keep_ids:
        # Makes each sequence ID part of a split containing only that sequence
        # ID, so all sequence IDs will be kept
        split_map = IdentityDict()
    else:
        # By default, everything part of the same split
        split_map = ConstantDict(None)

    with a.input_fasta as fp:
        sequences = parse_fasta(fp)
        dedup = deduplicate_sequences(sequences, counts, split_map)

    deduplicated = ((i.id, i.sequence) for i in dedup)
    with a.output_fasta as fp:
        write_fasta(deduplicated, fp)

    with a.map_out as fp:
        write_map(dedup, fp)

if __name__ == '__main__':
    main()

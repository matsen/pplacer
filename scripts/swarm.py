#!/usr/bin/env python

"""Dereplicate, pool, and cluster reads using swarm
(https://github.com/torognes/swarm), producing output suitable for
subsequent analysis by pplacer.

Input sequence names should *not* contain abundance annotations (these
are added before the sequences are provided to swarm). Output sequence
names are unannotated as well. Input files may be compressed
(compression is detected according to a file sufffix of either .bz2 or
.gz).

The output file specified by -a/--abundances contains three columns:

 1. the name of a seed sequence representing cluster C;
 2. the name of a sequence included in cluster C representing some specimen S;
 3. the number of reads representing cluster C originating from specimen S.

Let's name these columns ('seed', 'read_from_S', and 'abundance'). In
principle, the sum of the values in the column containing abundances
should equal the total number of input reads. However, swarm does not
allow sequences containing ambiguities, so these are silently
discarded.

In the specific case of pplacer, this file can be provided as an
argument to ``guppy redup -d`` after placement of sequences in
seeds.fasta to generate a placefile reflecting the original read
masses before clustering.

But more generally, this output, along with a specimen map and
assignment of seed sequences to taxon names, can be used to construct
a table describing taxon abundance by specimen.

Consider these two other tables:

 * 'specimen_map', a table provided as input to this script with
   columns ('read_from_S', 'specimen')
 * 'assignments', a table assigning clusters to taxa with columns
   ('seed', 'taxon').

Given these three inputs, a taxon table can be constructed as follows
(using SQL to illustrate the relations)::

  select specimen, taxon, sum(abundance) as read_count
  from abundances join specimen_map using (read_from_S)
  join assignments using(seed)
  group by specimen, taxon;

::

"""

from __future__ import print_function
from collections import namedtuple, defaultdict
from tempfile import NamedTemporaryFile as ntf
from distutils.version import LooseVersion
import argparse
import csv
import gzip
import os
import subprocess
import sys
try:
    import bz2
except ImportError:
    bz2 = None


class Opener(object):

    """Factory for creating file objects

    Keyword Arguments:
        - mode -- A string indicating how the file is to be opened. Accepts the
            same values as the builtin open() function.
        - bufsize -- The file's desired buffer size. Accepts the same values as
            the builtin open() function.
    """

    def __init__(self, mode='r', bufsize=-1):
        self._mode = mode
        self._bufsize = bufsize

    def __call__(self, string):
        if string is sys.stdout or string is sys.stdin:
            return string
        elif string == '-':
            return sys.stdin if 'r' in self._mode else sys.stdout
        elif string.endswith('.bz2'):
            if bz2 is None:
                raise ImportError('could not import bz2 module - was python built with libbz2?')
            return bz2.BZ2File(string, self._mode, self._bufsize)
        elif string.endswith('.gz'):
            return gzip.open(string, self._mode, self._bufsize)
        else:
            return open(string, self._mode, self._bufsize)

    def __repr__(self):
        args = self._mode, self._bufsize
        args_str = ', '.join(repr(arg) for arg in args if arg != -1)
        return '{}({})'.format(type(self).__name__, args_str)


SeqLite = namedtuple('SeqLite', 'id, description, seq')


def fastalite(handle):
    """
    Lightweight fasta parser. Returns iterator of namedtuple instances
    with fields (id, description, seq) given file-like object ``handle``.
    """

    name, seq = '', ''
    for line in handle:
        if line.startswith('>'):
            if name:
                yield SeqLite(name.split()[0], name, seq)
            name, seq = line[1:].strip(), ''
        else:
            seq += line.strip()

    if name and seq:
        yield SeqLite(name.split()[0], name, seq)


def parse_name(name):
    key, abundance = name.rsplit('_', 1)
    return key, int(abundance)


def check_swarm_version(min_version):
    output = subprocess.check_output('swarm -v 2>&1; true', shell=True)
    version = output.split()[1].decode(encoding='UTF-8')
    if LooseVersion(version) < LooseVersion(min_version):
        sys.exit('Error: swarm version >= {} is required, '
                 'found swarm version {}'.format(min_version, version))


def main(arguments):
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)

    parser.add_argument(
        'infile', type=Opener('r'),
        help="Input file containing trimmed reads in fasta format")
    parser.add_argument(
        'specimen_map', type=Opener('r'),
        help="headless csv file with columns (read, specimen)")

    parser_outfiles = parser.add_argument_group('output files')
    parser_outfiles.add_argument(
        '-w', '--seeds', type=Opener('w'),
        help="Output fasta file containing OTU representatives")
    parser_outfiles.add_argument(
        '-a', '--abundances', type=Opener('w'),
        help="csv file providing abundances by specimen")
    parser_outfiles.add_argument(
        '--tmpdir', help="""optional directory name for creating
        temporary intermediate files (created in system temp file and
        discarded by default)""")

    parser.add_argument('-d', '--differences', type=int, default=1)
    parser.add_argument('-t', '--threads', default=4, type=int)
    parser.add_argument('-m', '--min-mass', type=int, default=None, metavar='N',
                        help="drop OTUs with total mass less than N")

    args = parser.parse_args(arguments)

    check_swarm_version(min_version='2.1.2')

    delete = args.tmpdir is None

    # identifies specimen of origin (values) for each read (keys)
    specimen_map = dict(csv.reader(args.specimen_map))

    # Create an open file for each specimen. For each read, write a
    # sequences annotated with _1 (abundance = 1) if there are no
    # ambiguities.
    raw_reads = {specimen: ntf('w', prefix='{}-'.format(specimen),
                               suffix='.fasta', dir=args.tmpdir, delete=False)
                 for specimen in set(specimen_map.values())}

    for seq in fastalite(args.infile):
        if 'N' in seq.seq:
            continue
        raw_reads[specimen_map[seq.id]].write('>{}_1\n{}\n'.format(seq.id, seq.seq))

    # dereplicate each specimen and concatenate all remaining reads to
    # a single temporary file
    with ntf('w', prefix='d0all-', suffix='.fasta', dir=args.tmpdir, delete=delete) as d0all, \
         ntf('r+', prefix='otus-', suffix='.txt', dir=args.tmpdir, delete=delete) as clusters, \
         ntf('r+', prefix='otus-', suffix='.fasta', dir=args.tmpdir, delete=delete) as swarm_out, \
         open(os.devnull, 'w') as devnull:

        for specimen, infile in list(raw_reads.items()):
            with ntf(prefix='d0-', suffix='.fasta', dir=args.tmpdir, delete=False) as d0:
                infile.close()  # flush any pending writes from above
                cmd = ['swarm',
                       '--seeds', d0.name,
                       '-o', os.devnull,  # ordinarily to stdout, but we don't want it
                       '--differences', '0',
                       '-t', str(args.threads),
                       infile.name]
                subprocess.check_call(cmd, stderr=devnull)

            # concatenate to pooled file
            cmd = 'cat "{}" >> "{}"'.format(d0.name, d0all.name)
            subprocess.check_call(cmd, shell=True)

            # remove temporary intermediate files
            if delete:
                os.remove(infile.name)
                os.remove(d0.name)

        # cluster dereplicated, pooled reads
        cmd = ['swarm',
               '--seeds', swarm_out.name,
               '-o', clusters.name,
               '--differences', str(args.differences),
               '-t', str(args.threads),
               d0all.name]

        # print ' '.join(cmd)
        subprocess.check_call(cmd, stderr=devnull)
        clusters.seek(0)
        swarm_out.seek(0)

        writer = csv.writer(args.abundances)

        grand_total, keep_total = 0.0, 0.0
        for seq, line in zip(fastalite(swarm_out), clusters):
            otu_rep, total = parse_name(seq.id)
            names_and_counts = list(map(parse_name, line.split()))

            assert otu_rep == names_and_counts[0][0]
            assert total == sum(x[1] for x in names_and_counts)

            grand_total += total

            if args.min_mass is not None and total < args.min_mass:
                continue

            keep_total += total

            # associate each specimen with a list of read names and masses
            specimens = defaultdict(list)
            for name, count in names_and_counts:
                specimens[specimen_map[name]].append((name, count))

            # start with the specimen represented by the OTU representative
            specimen = specimen_map[otu_rep]
            names, counts = list(zip(*specimens.pop(specimen)))
            writer.writerow([otu_rep, otu_rep, sum(counts)])

            # ... then iterate over other specimens with reads in this OTU
            for specimen, val in specimens.items():
                names, counts = list(zip(*val))
                writer.writerow([otu_rep, names[0], sum(counts)])

            # finally, write the OTU seed (with no abundance annotation)
            args.seeds.write('>{}\n{}\n'.format(otu_rep, seq.seq))

    print('total yield:', round(100.0 * keep_total/grand_total, 2))

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))


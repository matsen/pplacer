#!/usr/bin/env python
import collections
import subprocess
import itertools
import argparse
import tempfile
import logging
import os.path
import sqlite3
import atexit
import shutil
import errno
import shlex
import csv

from taxtastic.refpkg import Refpkg
from Bio import SeqIO

log = logging.getLogger(__name__)

def logging_check_call(cmd, *a, **kw):
    log.info(' '.join(cmd))
    return subprocess.check_call(cmd, *a, **kw)

def silently_unlink(path):
    try:
        os.unlink(path)
    except OSError, e:
        if e.errno != errno.ENOENT:
            raise

input_suffixes = {'align-each': 'fasta', 'merge-each': 'sto', 'none': 'sto'}

def main():
    logging.basicConfig(
        level=logging.INFO, format="%(levelname)s: %(message)s")

    parser = argparse.ArgumentParser(
        description="Classify query sequences using a hrefpkg.")
    parser.add_argument('hrefpkg', help="hrefpkg to classify using")
    parser.add_argument('query_seqs', help="input query sequences")
    parser.add_argument('classification_db', help="output sqlite database")
    parser.add_argument('-j', '--ncores', default=1, type=int, metavar='CORES',
                        help="number of cores to tell commands to use")
    parser.add_argument('-r', '--classification-rank', metavar='RANK',
                        help="rank to perform the initial NBC classification at")
    parser.add_argument('--classifier', default='pplacer',
                        help="which classifier to use with guppy classify")
    parser.add_argument('--pplacer-args', default=[], type=shlex.split,
                        help="additional arguments for pplacer")
    parser.add_argument('--classification-args', default=[], type=shlex.split,
                        help="additional arguments for guppy classification")
    parser.add_argument('--post-prob', default=False, action='store_true',
                        help="place with posterior probabilities")
    parser.add_argument('--workdir', metavar='DIR',
                        help=("directory to write intermediate files to "
                              "(default: a temporary directory)"))
    parser.add_argument('--disable-cleanup', default=False, action='store_true',
                        help="don't remove the work directory as the final step")
    parser.add_argument('--use-mpi', default=False, action='store_true',
                        help="run refpkg_align with MPI")
    parser.add_argument(
        '--alignment', choices=['align-each', 'merge-each', 'none'], default='align-each',
        help=("respectively: align each input sequence; subset an input stockholm "
              "alignment and merge each sequence to a reference alignment; only subset "
              "an input stockholm alignment (default: align-each)"))
    parser.add_argument(
        '--cmscores', type=argparse.FileType('w'), metavar='FILE',
        help="in align-each mode, write out a file containing the cmalign scores")

    programs = parser.add_argument_group("external binaries")
    programs.add_argument(
        '--pplacer', default='pplacer', metavar='PROG', help="pplacer binary to call")
    programs.add_argument(
        '--guppy', default='guppy', metavar='PROG', help="guppy binary to call")
    programs.add_argument(
        '--rppr', default='rppr', metavar='PROG', help="rppr binary to call")
    programs.add_argument(
        '--refpkg-align', default='refpkg_align.py', metavar='PROG',
        help="refpkg_align binary to call")
    programs.add_argument(
        '--cmalign', default='cmalign', metavar='PROG', help="cmalign binary to call")


    args = parser.parse_args()

    if args.ncores < 0:
        args.error('ncores must be >= 0')

    mpi_args = []
    if args.use_mpi and args.ncores > 0:
        mpi_args = ['--use-mpi', '--mpi-arguments', '-np %d' % (args.ncores,)]

    if args.workdir is None:
        workdir = tempfile.mkdtemp()
    else:
        workdir = args.workdir
        try:
            os.makedirs(workdir)
        except OSError, e:
            if e.errno != errno.EEXIST:
                raise

    if not args.disable_cleanup:
        @atexit.register
        def cleanup_workdir():
            shutil.rmtree(workdir, ignore_errors=True)

    classif_db = os.path.join(workdir, 'classifications.sqlite')
    index_refpkg = os.path.join(args.hrefpkg, 'index.refpkg')
    index = Refpkg(index_refpkg)
    index_rank = index.metadata('index_rank')
    classif_rank = args.classification_rank or index_rank
    index_counts = os.path.join(args.hrefpkg, 'index-%s.counts' % (classif_rank,))
    log.info('performing initial classification at %s', classif_rank)
    silently_unlink(classif_db)
    logging_check_call(
        [args.rppr, 'prep_db', '--sqlite', classif_db, '-c', index_refpkg])
    logging_check_call(
        [args.guppy, 'classify', '--sqlite', classif_db, '-c', index_refpkg,
         '--classifier', 'nbc', '--nbc-rank', classif_rank, '--no-pre-mask',
         '--nbc-sequences', args.query_seqs, '--nbc-counts', index_counts,
         '-j', str(args.ncores)])

    with open(os.path.join(args.hrefpkg, 'index.csv'), 'rU') as fobj:
        refpkg_map = dict(csv.reader(fobj))

    log.info('determining bins at %s', index_rank)
    conn = sqlite3.connect(classif_db)
    curs = conn.cursor()
    curs.execute("""
        SELECT name,
               tax_id
        FROM   multiclass
        WHERE  want_rank = ?
               AND rank = want_rank
    """, (index_rank,))
    seq_bins = dict(curs)
    all_bins = set(seq_bins.itervalues()) & set(refpkg_map)
    log.info('classified into %d bins', len(all_bins))
    log.debug('bins: %r', all_bins)
    bin_outputs = {}
    bin_counts = collections.Counter()

    input_suffix = input_suffixes[args.alignment]
    def binfile(bin):
        return os.path.join(workdir, '%s.%s' % (bin, input_suffix))

    for bin in all_bins:
        bin_outputs[bin] = open(binfile(bin), 'w')

    if args.alignment == 'align-each':
        for seq in SeqIO.parse(args.query_seqs, 'fasta'):
            bin = seq_bins.get(seq.id)
            if bin is None or bin not in bin_outputs:
                continue
            SeqIO.write([seq], bin_outputs[bin], 'fasta')
            bin_counts[bin] += 1

    # Gross ad-hoc stockholm parser. This is only necessary because biopython
    # can't handle the metadata that cmalign adds to the file.
    else:
        def write_all(lines):
            for line in lines:
                for fobj in bin_outputs.itervalues():
                    fobj.write(line)

        with open(args.query_seqs, 'r') as fobj:
            # Copy the stockholm header,
            write_all(itertools.islice(fobj, 3))

            # then partition the sequences,
            for line in fobj:
                # (stopping if we reach the metadata at the end (hopefully it's at
                # the end))
                if line.startswith('#=GC'):
                    break
                id, seq = line.split()
                bin = seq_bins.get(id)
                if bin is None or bin not in bin_outputs:
                    continue
                bin_outputs[bin].write(line)
                bin_counts[bin] += 1

            # and then copy the footer.
            write_all([line])
            write_all(fobj)

    for fobj in bin_outputs.itervalues():
        fobj.close()

    # This is a bit nasty but I think it's Good Enough. We can change it if we
    # encounter problems or need to be more portable.
    if args.cmscores is not None:
        cmscores_proc = subprocess.Popen(
            ['romp', 'cmscores', '/dev/stdin'],
            stdin=subprocess.PIPE, stdout=args.cmscores)
        cmscores_args = [
            '--alignment-method', 'INFERNAL',
            '--stdout', '/dev/fd/%d' % (cmscores_proc.stdin.fileno(),)]
        cmscores_stdout = cmscores_proc.stdout
    else:
        cmscores_proc = None
        cmscores_args = []
        cmscores_stdout = open(os.devnull, 'w')

    for e, bin in enumerate(all_bins):
        log.info('classifying bin %s (%d/%d; %d seqs)',
                 bin, e + 1, len(all_bins), bin_counts[bin])
        input = binfile(bin)
        aligned = os.path.join(workdir, bin + '-aligned.sto')
        placed = os.path.join(workdir, bin + '.jplace')
        refpkg = os.path.join(args.hrefpkg, refpkg_map[bin])

        if args.alignment == 'align-each':
            logging_check_call(
                [args.refpkg_align, 'align', '--output-format', 'stockholm',
                 refpkg, input, aligned] + mpi_args + cmscores_args)
        elif args.alignment == 'merge-each':
            refpkg_obj = Refpkg(refpkg)
            logging_check_call(
                [args.cmalign, '--merge', '-o', aligned, '-1', '--hbanded',
                 '--sub', '--dna', refpkg_obj.resource_path('profile'),
                 refpkg_obj.resource_path('aln_sto'), input],
                stdout=cmscores_stdout)
        elif args.alignment == 'none':
            raise NotImplementedError('none')

        classifier_args, pplacer_args = [], []
        if args.classifier.startswith('nbc') or args.classifier.startswith('hybrid'):
            if '--no-pre-mask' not in args.classification_args:
                classifier_args.append('--no-pre-mask')
            classifier_args.extend(['--nbc-sequences', input])
        if args.post_prob:
            classifier_args.append('--pp')
            pplacer_args.extend(
                ['-p', '--inform-prior', '--prior-lower', '0.01'])

        logging_check_call(
            [args.pplacer, '--discard-nonoverlapped', '-c', refpkg,
             '-j', str(args.ncores), aligned, '-o', placed]
            + args.pplacer_args
            + pplacer_args)

        logging_check_call(
            [args.guppy, 'classify', '--sqlite', classif_db, '-c', refpkg, placed,
             '-j', str(args.ncores), '--classifier', args.classifier]
            + args.classification_args
            + classifier_args)

    if cmscores_proc is not None:
        cmscores_proc.communicate()

    log.info('cleaning up `multiclass` table')
    conn.rollback()
    curs = conn.cursor()
    curs.execute("""
        CREATE TEMPORARY TABLE classified AS
          SELECT name,
                 COUNT(DISTINCT run_id) n_runs
            FROM multiclass
                 JOIN placements USING (placement_id)
           GROUP BY name
          HAVING n_runs > 1
    """)
    curs.execute("SELECT name FROM classified WHERE n_runs > 2")
    too_many_classifications = [name for name, in curs]
    if too_many_classifications:
        raise ValueError("some sequences got classified more than twice",
                         too_many_classifications)
    curs.execute("""
        DELETE FROM multiclass
         WHERE (SELECT run_id
                  FROM placements p
                 WHERE p.placement_id = multiclass.placement_id) = 1
           AND name IN (SELECT name
                          FROM classified)
    """)
    conn.commit()

    shutil.copy(classif_db, args.classification_db)

main()

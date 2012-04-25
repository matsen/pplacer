import subprocess
import argparse
import tempfile
import logging
import os.path
import sqlite3
import atexit
import shutil

from taxtastic.refpkg import Refpkg
from Bio import SeqIO

log = logging.getLogger(__name__)

def main():
    logging.basicConfig(
        level=logging.INFO, format="%(levelname)s: %(message)s")

    parser = argparse.ArgumentParser()
    parser.add_argument('hrefpkg')
    parser.add_argument('query_seqs')
    parser.add_argument('classification_db')
    parser.add_argument('--pplacer', default='pplacer')
    parser.add_argument('--guppy', default='guppy')
    parser.add_argument('--rppr', default='rppr')
    parser.add_argument('--refpkg-align', default='refpkg_align.py')
    parser.add_argument('--disable-cleanup', default=False, action='store_true')

    args = parser.parse_args()
    workdir = tempfile.mkdtemp()
    if not args.disable_cleanup:
        @atexit.register
        def cleanup_workdir():
            shutil.rmtree(workdir, ignore_errors=True)

    classif_db = os.path.join(workdir, 'classifications.sqlite')
    index_refpkg = os.path.join(args.hrefpkg, 'index.refpkg')
    index = Refpkg(index_refpkg)
    index_rank = index.metadata('index_rank')
    log.info('performing initial classification at %s', index_rank)
    subprocess.check_call(
        [args.rppr, 'prep_db', '--sqlite', classif_db, '-c', index_refpkg])
    subprocess.check_call(
        [args.guppy, 'classify', '--sqlite', classif_db, '-c', index_refpkg,
         '--classifier', 'nbc', '--nbc-rank', index_rank,
         '--no-pre-mask', '--nbc-sequences', args.query_seqs])

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
    all_bins = set(seq_bins.itervalues())
    log.info('classified into %d bins', len(all_bins))
    log.debug('bins: %r', all_bins)
    bin_outputs = {}
    for bin in all_bins:
        bin_outputs[bin] = open(os.path.join(workdir, bin + '.fasta'), 'w')

    for seq in SeqIO.parse(args.query_seqs, 'fasta'):
        bin = seq_bins.get(seq.id)
        if bin is None:
            continue
        SeqIO.write([seq], bin_outputs[bin], 'fasta')

    for fobj in bin_outputs.itervalues():
        fobj.close()

    for e, bin in enumerate(all_bins):
        log.info('classifying bin %s (%d/%d)', bin, e + 1, len(all_bins))
        unaligned = os.path.join(workdir, bin + '.fasta')
        aligned = os.path.join(workdir, bin + '-aligned.sto')
        placed = os.path.join(workdir, bin + '.jplace')
        refpkg = os.path.join(args.hrefpkg, bin + '.refpkg')
        subprocess.check_call(
            [args.refpkg_align, 'align', '--output-format', 'stockholm',
             refpkg, unaligned, aligned])
        subprocess.check_call(
            [args.pplacer, '-c', refpkg, aligned, '-o', placed])
        subprocess.check_call(
            [args.guppy, 'classify', '--sqlite', classif_db, '-c', refpkg, placed])

    shutil.copy(classif_db, args.classification_db)

main()

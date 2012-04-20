import subprocess
import argparse
import tempfile
import os.path
import sqlite3

from Bio import SeqIO

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('hrefpkg')
    parser.add_argument('query_seqs')
    parser.add_argument('--pplacer', default='pplacer')
    parser.add_argument('--guppy', default='guppy')
    parser.add_argument('--rppr', default='rppr')
    parser.add_argument('--refpkg-align', default='refpkg_align.py')
    parser.add_argument('--index-rank', default='family')

    args = parser.parse_args()
    workdir = tempfile.mkdtemp()

    classif_db = os.path.join(workdir, 'classifications.sqlite')
    index_refpkg = os.path.join(args.hrefpkg, 'index.refpkg')
    subprocess.check_call(
        [args.rppr, 'prep_db', '--sqlite', classif_db, '-c', index_refpkg])
    subprocess.check_call(
        [args.guppy, 'classify', '--sqlite', classif_db, '-c', index_refpkg,
         '--classifier', 'nbc', '--nbc-rank', args.index_rank, '--no-pre-mask',
         '--nbc-sequences', args.query_seqs])

    conn = sqlite3.connect(classif_db)
    curs = conn.cursor()
    curs.execute("""
        SELECT name,
               tax_id
        FROM   multiclass
        WHERE  want_rank = ?
               AND rank = want_rank
    """, (args.index_rank,))
    seq_bins = dict(curs)
    all_bins = set(seq_bins.itervalues())
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

    for bin in all_bins:
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

    print workdir

main()

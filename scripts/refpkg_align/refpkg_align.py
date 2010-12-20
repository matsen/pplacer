#!/usr/bin/env python
import sys, os, string
from Bio import SeqIO, AlignIO
from Bio.Seq import Seq, SeqRecord


refpkg["aln_fasta"]

# i would like to specify a refpkg and a set of sequences to align as positional arguments like
# refpkg_align COG0003.refpkg to_align.fasta
# an -o option provides an out prefix, which defaults to a concatenation of the refpkg name and the name of the sequences to align, so in this case
out_prefix = "COG0003.to_align"

# say we have slurped the corresponding CONTENTS.json file into a dictionary called refpkg.

# step zero: find a temporary filename that we can write to safely. here we just use something dumb.
aligned = "tmp.afa"

# step one: run hmmalign, which can be found in /mnt/silo/silo_researcher/Matsen_F/MatsenGrp/local/bin
# here we would run it like
#hmmalign --outformat afa -o tmp.afa --mapali COG0003.refpkg/COG0003.ref.sth COG0003.refpkg/COG0003.ref.hmm to_align.fasta 
# which in terms of the dictionary would be
#hmmalign --outformat afa -o tmp.afa --mapali refpkg["aln_sto"] refpkg["profile"] to_align.fasta 

# step two: split off the reference sequences from the fragments.
# hmmalign puts them together, which is good that they then appear in the same alignment frame, but bad in that we want them pulled into separate files
# here are the file names for the out files:
out_refs = out_prefix+".ref.fasta"
out_frags = out_prefix+".frag.fasta"

# here is a function for getting a set of ids
def names(records):
    """"get the sequence names."""
    s = set()
    for record in records:
        s.add(record.id)
    return(s)

frag_names = names(SeqIO.parse("to_align.fasta", "fasta"))

# here is where we filter the seqs
def id_filter(f):
    for record in in_seqs:
	if (f(record.id)):
	    yield record

# this is not terribly elegant, in that we start from the beginning two times
# but i don't really mind.

# if we were using file objects rather than generator functions we could have 
# if (f(record.id)):
#     <write in out_frags>
# else:
#     <write in out_refs>

in_seqs = SeqIO.parse(aligned, "fasta")
SeqIO.write(id_filter(lambda(idstr): idstr not in frag_names), out_refs, "fasta")
in_seqs = SeqIO.parse(aligned, "fasta")
SeqIO.write(id_filter(lambda(idstr): idstr in frag_names), out_frags, "fasta")

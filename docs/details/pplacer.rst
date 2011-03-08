
Introduction
------------
Pplacer places query sequences on a fixed reference phylogenetic tree according to a reference alignment. 
In maximum likelihood (ML) mode, pplacer tries to find the attachement location and the pendant branch length which maximize the likelihood of the tree with pendant branch length attached.
In Bayesian posterior probability (PP) mode, pplacer tries to find the edge attachment which maximizes the posterior probability of a fragment placement on an edge conditioned on the reference tree (with branch lengths). 

A basic pplacer run looks like::

  pplacer -c my.refpkg aln.fasta

with a reference package (preferred), or without a reference package::

  pplacer -t reference_tree -s statistics_file aln.fasta


Sequence Alignment
------------------

There are several options and formats for providing alignments of reference and query sequences. Examples below illustrate various steps in the sequence alignment process.

Infernal: creating a reference sequence
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The first step in any pipeline involving Infernal (assuming you already have an alignment profile) is to create an alignment of reference sequences (see the Infernal docs for a description of options not mentioned here), for example::

  cmalign --hbanded --sub --dna -1 -o refalign.sto profile.cm refseqs.fasta 

Inputs to this command include an alignment profile (`profile.cm`) and unaligned reference sequences (`refs.fasta`). The output file, identified using the `-o` option, contains the aligned reference sequences in Stockholm format. The `-1` (that's a one, not an L) specifies non-interleaved output, one sequence per line.


Infernal: merged reference and query sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Query sequences must be aligned with respect to the reference sequences. This is easily accomplished using two calls to cmalign. First, align the query sequences just like the reference sequences above::

  cmalign --hbanded --sub --dna -1 -o qalign.sto profile.cm qseqs.fasta 

Next, merge the reference and query alignments using the `--merge` option.

  cmalign --merge --hbanded --sub --dna -1 -o merged.sto profile.cm refalign.sto qalign.sto

Now `merged.sto` contains a single alignment of both reference and query sequences, and can be used with pplacer as follows after making building a reference tree::

  pplacer ...

A closely related example involves alignment with an alignment profile and reference sequences included in a reference package (`my.refpkg` - note that names may vary in a reference package). So instead of the previous merge command, use::

  cmalign --merge --hbanded --sub --dna -1 -o mergedWithRefpkg.sto profile.cm my.refpkg/refalign.sto qalign.sto

Now it is even easier to call pplacer::

  pplacer -c my.refpkg mergedWithRefpkg.sto


.. Fantasy baseball
.. ----------------
.. 
.. Set to a nonzero value to run in fantasy baseball mode. 
.. The value given will be the desired average difference between the likelihood of the best placement with the given baseball parameters and that evaluating all
.. max-pitches pitches. 


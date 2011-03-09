
Introduction
------------
Pplacer places query sequences on a fixed reference phylogenetic tree according to a reference alignment. 
In maximum likelihood (ML) mode, pplacer tries to find the attachment location and the pendant branch length which maximize the likelihood of the tree with pendant branch length attached.
In Bayesian posterior probability (PP) mode, pplacer tries to find the edge attachment which maximizes the posterior probability of a fragment placement on an edge conditioned on the reference tree (with branch lengths). 

A basic pplacer run looks like::

  pplacer -c my.refpkg aln.fasta

with a reference package (preferred), or without a reference package::

  pplacer -t reference_tree -s statistics_file aln.fasta


Sequence Alignment
------------------

There are several options and formats for providing alignments of reference and query sequences.
Examples below illustrate various steps in the sequence alignment process.

Examples using Infernal
```````````````````````

Infernal_ is an excellent package for searching and aligning sequences using RNA secondary structure information.

Creating a reference alignment
''''''''''''''''''''''''''''''

The first step in any pipeline involving Infernal (assuming you already have an alignment profile but are not working from a reference package) is to create an alignment of reference sequences. 
See the Infernal docs for a description of options not mentioned here. 
For example::

  cmalign --hbanded --sub --dna -1 -o refalign.sto profile.cm refseqs.fasta 

Inputs to this command include an alignment profile (`profile.cm`) and unaligned reference sequences (`refs.fasta`).
The output file, identified using the `-o` option, contains the aligned reference sequences in Stockholm format.
The `-1` (that's a one, not an L) specifies non-interleaved output, one sequence per line.


Merging reference and query sequences
'''''''''''''''''''''''''''''''''''''

Query sequences must be aligned with respect to the reference sequences.
This is easily accomplished using two calls to cmalign.
First, align the query sequences just like the reference sequences above::

  cmalign --hbanded --sub --dna -1 -o qalign.sto profile.cm qseqs.fasta 

Next, merge the reference and query alignments using the `--merge` option::

  cmalign --merge --hbanded --sub --dna -1 -o merged.sto profile.cm refalign.sto qalign.sto

Now `merged.sto` contains a single alignment of both reference and query sequences, and can be used with pplacer as follows after making a reference tree and accompanying statistics file::

  pplacer -t reference_tree -a statistics_file merged.sto

Using a reference package
'''''''''''''''''''''''''

A closely related example involves alignment with the profile and reference sequences included in a reference package (`my.refpkg` - note that names may vary in a reference package).
So now we skip creation of the reference alignment.
First, create the query alignment::

  cmalign --hbanded --sub --dna -1 -o qalign.sto my.refpkg/profile.cm qseqs.fasta 

...then merge::

  cmalign --merge --hbanded --sub --dna -1 \
    -o mergedWithRefpkg.sto \
    my.refpkg/profile.cm my.refpkg/refalign.sto qalign.sto

Now it is even easier to write the pplacer command::

  pplacer -c my.refpkg mergedWithRefpkg.sto


Examples using HMMER
````````````````````

HMMER_ is another excellent package for searching and aligning sequences by the Eddy group, which can align amino acid and nucleotide sequences.

Assume that we have a reference alignment `refseqs.sto` in Stockholm format. We first build an HMM::

  hmmbuild refseqs.hmm refseqs.sto

Then we can use it to make a combined alignment with the reference sequences and the reads::

  hmmalign -o combo.sto --mapali refseqs.sto refseqs.hmm qseqs.fasta 

Now we can run pplacer::

  pplacer -t rpoB.tre -s RAxML_info.rpoB combo.sto 

... or with a reference package::

  pplacer -c rpoB.refpkg combo.sto 


.. Fantasy baseball
.. ----------------
.. 
.. Set to a nonzero value to run in fantasy baseball mode. 
.. The value given will be the desired average difference between the likelihood of the best placement with the given baseball parameters and that evaluating all
.. max-pitches pitches. 


.. _Infernal: http://infernal.janelia.org/
.. _HMMER: http://hmmer.janelia.org/


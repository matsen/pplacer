
Introduction
------------
Pplacer places query sequences on a fixed reference phylogenetic tree according to a reference alignment. 
In maximum likelihood (ML) mode, pplacer tries to find the attachement location and the pendant branch length which maximize the likelihood of the tree with pendant branch length attached.
In Bayesian posterior probability (PP) mode, pplacer tries to find the edge attachment which maximizes the posterior probability of a fragment placement on an edge conditioned on the reference tree (with branch lengths). 

A basic pplacer run looks like::

  pplacer -c my.refpkg aln.fasta

with a reference package (preferred), or without a reference package::

  pplacer -t reference_tree -s statistics_file aln.fasta


.. Fantasy baseball
.. ----------------
.. 
.. Set to a nonzero value to run in fantasy baseball mode. 
.. The value given will be the desired average difference between the likelihood of the best placement with the given baseball parameters and that evaluating all
.. max-pitches pitches. 


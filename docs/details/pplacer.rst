
Introduction
------------
Pplacer places query sequences on a fixed reference phylogenetic tree according to a reference alignment.
In maximum likelihood (ML) mode, pplacer tries to find the attachment location and the pendant branch length which maximize the likelihood of the tree with pendant branch length attached.
In Bayesian posterior probability (PP) mode, pplacer tries to find the edge attachment which maximizes the posterior probability of a fragment placement on an edge conditioned on the reference tree (with branch lengths).

A basic pplacer run looks like::

  pplacer -c my.refpkg aln.fasta

with a reference package.
Running pplacer with a reference package is simple and produces taxonomic annotations which can be used for classification and visualization (see, for example classify_ and fat_).
If you wish to override specific parts of the reference package, you can do so with command line options.

A "reference package" is simply a collection of files including a reference tree, a reference alignment, and associated taxonomic information.
We have the beginnings of a `reference package database`_ which we hope in the future to be a comprehensive resource for phylogenetic placement.
It's just started, and we would love to have your submissions.
For now most users will have to make a reference package using our taxtastic_ package to take advantage of the taxonomic annotation features of pplacer.

The ``aln.fasta`` is the alignment file.
It should have the reference sequences which were used to make the reference tree, aligned with the query sequences to be placed on the reference tree.
The reference sequences must have identical names to those used in the reference tree.
It's possible to split the alignment into two files (the reference sequences and the query sequences); in that case you need to make sure that those alignments are in the same frame and have the same length.
This splitting was obligatory in v1.0.
The alignment can be in FASTA format (with a ``.fasta`` or ``.fa`` suffix) or Stockholm format (with a ``.sto`` or ``.sth`` suffix).

Another way to run pplacer is without a reference package::

  pplacer -t reference_tree -s statistics_file aln.fasta

The ``statistics_file`` is a file describing the evolutionary model used to make the reference tree (described in the section on reference tree below).
Running pplacer in this way will disable the taxonomic annotation features of pplacer v1.1.


Migrating from pplacer v1.0
---------------------------
There are a couple of differences between the present version and the previous version of pplacer which are worth knowing about.

* Rather than having a plain text ".place" output file, we now have a JSON_-format file which contains the placements
* Reference packages encapsulate reference information, making it easy to do placement and taxonomic annotation
* Alignments can now be supplied as a single file, rather than being split into reference and query alignments
* Much better: faster, taxonomic integration, etc
* ``placeviz``, ``placeutil`` and ``mokaphy`` have been replaced by a single binary called ``guppy``
* ``rppr`` binary for preparing reference packages


Pre-masking
-----------

Columns that are all gap in either the reference alignment or the query alignment do not impact the relative likelihood of placements.
Thus, starting in v1.1alpha08, we don't compute them at all.
This speeds things up a bunch, and uses a lot less memory.
We call this pre-masking, and it can be disabled with the ``--no-pre-mask`` flag.


JSON_ format specification
--------------------------

The new JSON format is very simple. Each document is a JSON object with a minimum of four keys: ``tree``, ``fields``,
``placements``, and ``version``. Another key, ``metadata``, is optional. Other keys in the root object are ignored.

===================  =====
Key                  Value
===================  =====
``version``          The version of the JSON format as an integer.
``tree``             The reference tree as a string, in "edge-numbered Newick" format.
``placements``       An array of placements.
``fields``           An array of strings corresponding to the data given in the placements array.
``metadata``         An object containing metadata about the generation of this collection of placements.
===================  =====

An "edge-numbered Newick" tree is simply a Newick format tree with integers in
curly braces which provide a well-defined numbering of edges. These edge
numbers are used to specify the edges on which the placements lie.

Currently there are two versions of the placefile format accepted by ``guppy``
and ``rppr``: ``1``, and ``2``, though only version 2 will be generated. There
are only two differences between versions 1 and 2: the format of the Newick
tree in the ``tree`` field has changed, and ``marginal_prob`` was renamed to
``marginal_like`` in version 2.

Version 1 used a slightly different version of edge-numbered Newick trees for
the ``tree`` field, where edge numbers were specified in square brackets
instead of curly braces. Both this kind of Newick tree and version 1 of the
JSON format are now deprecated.

The pplacer suite currently uses the following field names:

===================== ===========
Field                 Description
===================== ===========
``edge_num``          The edge number from the provided ``tree`` as an integer.
``likelihood``        ML log likelihood as a float.
``like_weight_ratio`` ML likelihood weight ratio as a float.
``distal_length``     ML distance from the distal side of the edge as a float.
``pendant_length``    ML pendant branch length as a float.
``post_prob``         The posterior probability of a placement on the edge.
``marginal_like``     The marginal likelihood of a placement on the edge.
``classification``    The ``tax_id`` from a reference package as a string.
``map_ratio``         The percent identity between this sequence and the corresponding MAP sequence.
``map_overlap``       The number of overlapping sites between this sequence and the corresponding MAP sequence.
===================== ===========

For ``guppy`` to be able to load a JSON file, it must have ``edge_num``,
``likelihood``, ``like_weight_ratio``, ``distal_length``, and
``pendant_length`` fields. All other fields are optional, but if one of
``post_prob`` and ``marginal_like`` are specified, both must be specified.

Each entry in the ``placements`` array is an object with the following keys:

===== =====
Key   Value
===== =====
``n`` A string or array of strings corresponding to the name or names of the sequences placed here.
``p`` An array of arrays containing placement data in the same order as ``fields``.
``m`` (optional) A float that represents the mass of this placement. If this key is specified, ``n`` must only be or contain a single string.
===== =====

An example JSON document follows, with the first placement showing uncertainty
in location, and the second showing two reads that had identical placements::

    {
      "tree": "((A:2[0],B:9[1]):7[2],C:5[3],D:1[4]):0[5];",
      "placements": [
        {"p":
          [[0, -1091.576026, 0.762438, 0.000008, 0.019642],
            [1, -10982.742117, 0.237562, 0.000008, 0.019579]
          ], "n": ["GLKT0ZE01CQ1P1"]
        },
        {"p": [[2, -1061.467623, 1.0, 0.003555, 0.000006]],
         "n": ["GLKT0ZE01C36CH", "GLKT0ZE01A0IBO"]}
      ],
      "metadata": {"invocation": "guppy to_json"},
      "version": 1,
      "fields": [
        "edge_num", "likelihood", "like_weight_ratio", "distal_length",
        "pendant_length"
      ]
    }

Making alignments for use with pplacer
--------------------------------------
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

Inputs to this command include an alignment profile (``profile.cm``) and unaligned reference sequences (``refs.fasta``).
The output file, identified using the ``-o`` option, contains the aligned reference sequences in Stockholm format.
The ``-1`` (that's a one, not an L) specifies non-interleaved output, one sequence per line.


Merging reference and query sequences
'''''''''''''''''''''''''''''''''''''

Query sequences must be aligned with respect to the reference sequences.
This is easily accomplished using two calls to cmalign.
First, align the query sequences just like the reference sequences above::

  cmalign --hbanded --sub --dna -1 -o qalign.sto profile.cm qseqs.fasta

Next, merge the reference and query alignments using the ``--merge`` option::

  cmalign --merge --hbanded --sub --dna -1 -o merged.sto profile.cm refalign.sto qalign.sto

Now ``merged.sto`` contains a single alignment of both reference and query sequences, and can be used with pplacer as follows after making a reference tree and accompanying statistics file::

  pplacer -t reference_tree -s statistics_file merged.sto

Using a reference package
'''''''''''''''''''''''''

A closely related example involves alignment with the profile and reference sequences included in a reference package (``my.refpkg`` - note that names may vary in a reference package).
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

Assume that we have a reference alignment ``refseqs.sto`` in Stockholm format. We first build an HMM::

  hmmbuild refseqs.hmm refseqs.sto

Then we can use it to make a combined alignment with the reference sequences and the reads::

  hmmalign -o combo.sto --mapali refseqs.sto refseqs.hmm qseqs.fasta

Now we can run pplacer::

  pplacer -t rpoB.tre -s RAxML_info.rpoB combo.sto

... or with a reference package::

  pplacer -c rpoB.refpkg combo.sto



Making reference trees
----------------------

FastTree
========

We save the log file so it can get parsed and become part of the reference package.

Nucleotide alignments
---------------------

FastTree should be used in the following way when making nucleotide reference trees for use with pplacer::

  FastTree -nt -gtr -log vaginal.log vaginal.fasta > vaginal.tre

In particular, do not use the ``-gamma`` option, but do use the ``-gtr`` option.

Amino Acid alignments
---------------------

FastTree should be used in the following way when making amino acid reference trees for use with pplacer::

  FastTree -log TIGR00001.log TIGR00001.fasta > TIGR00001.tre

Again, ``-gamma`` should not be used.


phyml and RAxML
===============

PHYML_ and RAxML_ are two nice packages for making ML trees that are supported for use with pplacer.
Pplacer only knows about the GTR, WAG, LG, and JTT models, so use those to build your trees.
If you are fond of another model and can convince me that I should implement it, I will.

Both of these packages implement gamma rate variation among sites, which accomodates that some regions evolve more quickly than others.
That's generally a good thing, but if you have millions of query sequences, you might have to run pplacer with fewer rate parameters to make it faster.

I run RAxML like so, on similar alignments (the "F" suffix on PROTGAMMAWAGF means to use the emperical amino acid frequencies)::

  raxmlHPC -m GTRGAMMA -n test -s nucleotides.phy
  raxmlHPC -m PROTGAMMAWAGF -n test -s amino_acids.phy

Even though Alexandros Stamatakis is quite fond of the "CAT" models and they accelerate tree inference, they aren't appropriate for use with pplacer.
We need to get an estimate of the gamma shape parameter.

PHYML can be run like so, on non-interleaved (hence the -q) phylip-format alignments::

  phyml -q -d nt -m GTR -i nucleotides.phy
  phyml -q -d aa -m WAG -i amino_acids.phy

Note that pplacer only works with phyml version 3.0 (the current version).

Both of these programs emit "statistics files": files that describe the phylogenetic model used.
Pplacer then uses those same statistics to place your reads.
For RAxML, they are called something like ``RAxML_info.test``, whereas for PHYML they are called something like ``test_aln_phyml_stats.txt``.

If your taxon names have too many funny symbols, pplacer will get confused.
We have had a difficult time with the wacky names exported by the otherwise lovely software geneious_.
If you have a tree which isn't getting parsed properly by pplacer, and you think it should be, send it to us and we will have a look.

Avoid giving pplacer a reference tree with lots of very similar sequences.
It's a waste of time-- pplacer must evaluate the resultant branches like any others.
Identical sequences are especially bad, and the resultant zero length branches will make pplacer complain.

If you give pplacer a reference tree which has been rooted in the middle of an edge, you will get a warning like::

  Warning: pplacer results make the most sense when the given tree is multifurcating
  at the root. See manual for details.

In pplacer the two edges coming off of the root have the same status as the rest of the edges; therefore they will counted as two separate edges.
That will lead to artifactually low likelihood weight ratio and posterior probabilities for query sequences placed on those edges.
This doesn't matter if your query sequences do not get placed next to the root, but you can avoid the problem altogether by rooting the tree at an internal node, or by leaving the outgroup in and rerooting the output trees.



Baseball
--------
"Baseball" is one way that pplacer substantially increases the speed of placement, especially on very large trees.
Baseball is a game where the player with the bat has a certain number of unsuccessful attempts, called "strikes", to hit the ball.

Pplacer applies this logic as follows.
Before placing placements, the algorithm gathers some extra information at each edge which makes it very fast to do a quick initial evaluation of those edges.
This initial evaluation of the edges gives the order with which those edges are evaluated in a more complete sense.
We will call full evaluations "pitches."
We start with the edge that looks best from the initial evaluation; say that the ML attachment to that edge for a given query has log likelihood L.
Fix some positive number D, which we call the "strike box."
We proceed down the list in order until we encounter the first placement which has log likelihood less than L - D, which we call a "strike."
Continue, allowing some number of strikes, until we stop doing detailed evaluation of what are most likely rather poor parts of the tree.

You can control the behavior of baseball playing using the ``--max-strikes``, ``--strike-box``, and ``--max-pitches`` options.
If, for any reason, you wish to disable baseball playing, simply add ``--max-strikes`` to zero (this also disables the ``--max-pitches`` option).

Having control over these options raises the question of how to set them.
The answer to this question can be given by pplacer's "fantasy baseball" feature.
To gain an understanding of the tradeoff between runtime and accuracy, it analyzes all ``--max-pitches`` best locations.
It then runs the baseball algorithm with each combination of strike box (from 0 to the specified ``--strike-box``) and max strikes (from 1 to the specified ``--max-strikes``).
Using these different settings the program reports

- the "batting average," i.e. the number of times the baseball algorithm with those settings achieved the optimal location obtained by evaluating all ``--max-pitches`` best locations; found in the file prefix.batting_avg.out
- the "log likelihood difference," i.e. the difference between the ML log likelihood achieved by the baseball algorithm with those settings compared to the best obtained by evaluating all ``--max-pitches`` best locations; found in the file prefix.like_diff.out
- the "number of trials," i.e. the number of locations fully evaluated by the baseball algorithm with those settings; found in the file prefix.n_trials.out

The fantasy mode is invoked by telling pplacer what average likelihood difference you would like via the ``--fantasy`` option.
You can also tell it to run an evenly-spaced fraction of the query sequences in fantasy mode using the ``--fantasy-frac`` option, which gives you an idea of the optimal run parameters for the rest of the sequences. For example::

  pplacer --max-strikes 10 --strike-box 10 --fantasy 0.05 --fantasy-frac 0.02 -r example.fasta...

says to run pplacer trying all of the combinations of max strikes and strike box up to 10, looking for the optimal combination which will give an average log likelihood difference of 0.05, and running on 2% of the query sequences.
If, for any reason, you wish to disable baseball playing, simply add ``--max-strikes`` to zero (this also disables the ``--max-pitches`` option).

You can use R to plot these matrices in a heat-map like fashion like so::

  ba > read.table("reads_nodups.batting_avg.out")
  image(x=c(0:nrow(ba)-1),xlab= "strike box", ylab= "number of strikes", \
     y=c(1:ncol(ba)-1),z=as.matrix(ba), main="batting average")

Note that we have set things up so that turning on posterior probability with ``-p`` now changes the default search parameters to make a deeper search as follows::

  --keep-at-most 20
  --keep-factor 0.001
  --max-strikes 20

You can set these to anything you like by using these flags *after* the ``-p``.


Fig ranking
-----------

"Fig ranking" is a way to reduce the number of initial comparisons done by
using the structure of the reference tree. This initial phase is not the
bottleneck for trees on a thousand or so taxa, but it is for trees on tens of
thousands of taxa or more.

If a value is specified as ``--fig-cutoff x``, pplacer will find subtrees of
the reference tree (that we call figs) on the reference tree such that no two
leaves in the cluster have a distance of greater than ``x``. Each leaf is
contained in exactly one fig. A representative edge of the fig is chosen as
follows: say *n* is the most proximal node contained in the fig; the
representative is the edge descending from *n* to the subtree with the greatest
number of leaves.

With a collection of figs, pplacer will rank each of the representative edges
by the initial evaluation likelihood given a query sequence. For each fig, in
this order, pplacer selects all of the edges within the fig as well as all of
the edges proximal to the fig up to the root of the tree. These edges are
ranked by the same initial evaluation likelihood before pplacer attempts to
place the query sequence on each in turn. No edge will be attempted twice; if
the same edge is proximal to two separate figs, it will only be attempted when
the first fig is evaluated.

As each fig is evaluated for a query sequence, pplacer will also select any
figs ordered immediately after the current fig where the difference between
that fig's representative likelihood and the current fig's representative
likelihood is less than the value of the ``--strike-box`` parameter. Each of
these figs' sets of edges are then merged into the current fig's edge set.

To test the accuracy of fig evaluation vs. full evaluation, the
``--fig-eval-all`` flag can be specified to do both fig and full evaluation,
then show the percentage of sequences where the best location chosen by full
evaluation and fig evaluation is the same. ``--fig-eval-all`` must be specified
to specify the ``--fig-eval-discrepancy-tree`` flag. If this flag is specified,
a tree will be written out showing the locations chosen by both methods for
each sequence where the two differ.

The colored trees written out by the ``--fig-tree`` and
``--fig-eval-discrepancy-tree`` flags shows figs as colored subtrees within the
reference tree.


.. _Infernal: http://infernal.janelia.org/
.. _HMMER: http://hmmer.janelia.org/
.. _reference package database: http://microbiome.fhcrc.org/apps/refpkg/
.. _taxtastic: http://github.com/fhcrc/taxtastic/
.. _JSON: http://www.json.org/
.. _PHYML: http://www.atgc-montpellier.fr/phyml/
.. _RAxML: http://sco.h-its.org/exelixis/software.html
.. _geneious: http://www.geneious.com/
.. _classify: guppy_classify.html
.. _fat: guppy_fat.html

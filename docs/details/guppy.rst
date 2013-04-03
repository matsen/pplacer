=====
guppy
=====

:Authors: Erick Matsen and Aaron Gallagher
:Title: guppy
:Version: 1.1
:License: GPL v3
:Date: September 2011

|guppy| is a tool for working with, visualizing, and comparing collections of phylogenetic placements, such as those made by pplacer_ or RAxML's EPA.
"GUPPY" is an acronym: Grand Unified Phylogenetic Placement Yanalyzer.

.. contents::
   :depth: 3
   :class: new

Introduction
============

To use the statistical comparison features of |guppy|, it's a good idea to have a basic understanding of what the Kantorovich-Rubinstein (KR, a.k.a. earth-mover's distance) is doing, and how the edge PCA and squash clustering algorithms work.
There is a gentle introduction in `Matsen and Evans`_, and a more full treatment in `Evans and Matsen`_.

Here's a table to demonstrate the relation of |guppy| concepts to ones which may be more familiar to the reader:


.. unweighted UniFrac        Phylogenetic diversity (pdfrac_)
.. OTU alpha diversity       PD of the subtree spanned by the placements (pd_)

========================  =============
familiar concept          guppy concept
========================  =============
weighted UniFrac          |KR| distance (kr_)
UPGMA using UniFrac       "squash" clustering (squash_)
PCA using UniFrac         Edge PCA (pca_)
========================  =============

This table does not show equivalences, but rather a list of hints for further exploration.
For example, the KR distance is really a generalization of weighted UniFrac, and edge PCA is a type of PCA that takes advantage of the special structure of phylogenetic placement data.
The heat tree (kr_heat_) and barycenter (bary_) have no analogs in previous types of phylogenetic microbial analysis.


Usage
=====

|guppy| does lots of different things-- it makes heat trees, makes matrices of Kantorovich-Rubinstein distances, does its own version of PCA, etc.
Each of these have their own options.
Rather than make a suite of little programs, we have opted for an interface analogous to git and svn: namely, a collection of different actions wrapped up into a single interface.

There are two ways to access the commands-- through the command line interface, and through the batch mode.
A list of these programs is below, and can always be found using ``guppy --cmds``.


Command line interface
----------------------
The general way to invoke |guppy| is ``guppy COMMAND [options] placefile[s]`` where COMMAND is one of the |guppy| commands.
For example::

  guppy heat --gray-black coastal.jplace DCM.jplace

These programs are listed with more detail below, and can always be found using ``guppy --cmds`` .

|guppy| can also be invoked as ``guppy --quiet COMMAND [...]``,  which prevents
the specified command from writing to stdout unless explicitly requested.

Batch mode
----------

It's easy to run lots of commands at once with batch mode.  However, unlike
running the equivalent set of commands on the command line, *placefiles are
only loaded once per batch file run*. |guppy| will load a given file the first
time it is used in a command.

Batch files are files with one guppy command per line, specified exactly as
would be written in a shell, except without the leading ``guppy``. Arguments
can be enclosed in double quotes to preserve whitespace, and double quotes
within quoted strings are quoted by doubling (e.g. ``"spam ""and"" eggs"``).
Globbing (e.g. ``*.jplace``) is not allowed. Comments are also allowed in batch
files; everything on a line after a ``#`` is ignored.

An example batch file::

  # Whole-line comment.
  pca -o pca -c some.refpkg src/a.jplace src/b.jplace
  squash -c some.refpkg -o squash_out src/a.jplace src/b.jplace
  classify -c some.refpkg some.jplace  # inline comment

If this was saved as ``example.batch``, it would be invoked from guppy as::

  guppy --batch example.batch

Advanced features
^^^^^^^^^^^^^^^^^

Batch files also have two unique features: virtual placefiles, and parameter
substitution.

Within a batch file, if a placefile is saved to or loaded from a path beginning
with a ``@``, the data will be stored in memory instead of written to disk. For
example::

  merge -o @merged.jplace src/a.jplace src/b.jplace
  info @merged.jplace

will effectively run ``guppy info`` on the placefile resulting from merging the
two arguments, but without ever writing that merged file to disk.

Additionally, parameters can be passed in from the command line to the batch
file. On the command line, parameters are specified as additional arguments to
guppy in ``key=value`` format. In the batch file, substitutions are done from
identifiers in ``{key}`` format. For example, when a batch file containing ::

  info {k1} {k2}

is invoked with ::

  guppy --batch example.batch k1=1.jplace k2=2.jplace

the impact will be the same as running ::

  guppy 1.jplace 2.jplace

Braces can also be quoted by doubling (e.g. ``{{foo}}`` will become ``{foo}``).


About multiplicities
--------------------

pplacer and guppy support "multiplicities," i.e. multiple reads being treated as one.
For example, if some reads are identical, they can be treated as a group.
Doing so makes guppy operations *much* faster.

By default, they are used "as is" for guppy calculations-- a single placement with multiplicity four is the same as four reads placed individually.
However, if one would like to decrease the impact of multiplicities on downstream analysis (e.g. if PCR artifacts are suspected) one can use the ``--transform`` option of ``mft`` to choose a transform for the multiplicies before use.
Doing so will convert your placements into labeled masses.


.. _split-placefiles:

'Split' placefiles
------------------

It's often convenient to split up place files in all sorts of ways, but it's nice not to have to duplicate the information in the placefiles multiple times.
For that reason, we have introduced "split" placefiles.
The syntax for these is ``my.jplace:my.csv``, where the CSV file maps from query sequence names to the split placefile name.
For example, say ``my.csv`` was like::

  "read1","a"
  "read2","b"

and ``my.jplace`` has placements for read1 and read2.
Then ``my.jplace:my.csv`` would act like a list of two placefiles named a and b, with read1 and read2, respectively.
Not every placement name in the place file needs to appear in the CSV file, so you can use this for subsetting.


BIOM files
----------

Any command which expects a placefile (in both guppy and rppr) can also be
given a `BIOM file`_. As BIOM files (unlike placefiles) do not contain a tree,
it must be passed at the same time, with a colon delimiting the two paths. For
example, to run ``guppy info`` on ``my.biom`` with a tree ``my.tre``, the
invocation would be ``guppy info my.tre:my.biom``. Trees must be in the Newick
format.

As the BIOM format describes counts at leaves, the placements generated by
parsing a BIOM file will all have zero distal branch length and zero pendant
branch length.

BIOM files will also automatically be split by sample. If a BIOM file has
columns ``Sample1`` and ``Sample2``, that file will be interpreted as two
placefiles, named respectively ``Sample1`` and ``Sample2``.


phyloXML viewing notes
----------------------
|guppy| makes fattened and annotated trees to visualize the results of various analyses.
We have chosen to use phyloXML as the format for these trees, as it has width and color tags for edges; if you see .xml files coming out of a guppy analysis that's what they are for.
We like looking at these trees using the tree viewer archaeopteryx_.
If you open archaeopteryx with the default settings, you will see *nothing interesting*, simply the reference tree.
You need to click on the "Colorize Branches" and "Use branch-width" check boxes.
If you don't see those check boxes, then use `this configuration file`_ (if you are going to copy and paste it click on "raw" first).


List of subcommands
===================

The following table provides links to more in-depth documentation for each
guppy subcommand:

.. command-table

.. toctree::
   :glob:
   :hidden:

   guppy_*


.. |guppy| replace:: ``guppy``
.. |pplacer| replace:: ``pplacer``
.. |KR| replace:: Kantorovich-Rubinstein

.. _kr: guppy_kr.html
.. _pd: pd.html
.. _squash: guppy_squash.html
.. _pca: guppy_pca.html
.. _kr_heat: guppy_kr_heat.html
.. _bary: guppy_bary.html

.. _pplacer: http://matsen.fhcrc.org/pplacer
.. _this configuration file: https://github.com/fhcrc/microbiome-demo/blob/master/bin/_aptx_configuration_file
.. _phyloxml: http://phyloxml.org/
.. _archaeopteryx: http://www.phylosoft.org/archaeopteryx/
.. _Evans and Matsen: http://arxiv.org/abs/1005.1699
.. _Matsen and Evans: http://arxiv.org/abs/1107.5095
.. _BIOM file: http://biom-format.org/

.. vim:set ai fo+=n fo-=l ft=rst:


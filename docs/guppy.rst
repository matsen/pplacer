=====
guppy
=====

:Authors: Erick Matsen and Aaron Gallagher
:Title: mokaphy
:Version: 0.4
:License: GPL v3
:Date: March 2011

|guppy| is a tool for working with, visualizing, and comparing collections of phylogenetic placements, such as those made by pplacer_ or RAxML's EPA.
"GUPPY" is an acronym: Grand Unified Phylogenetic Placement Yanalyzer.

.. contents::
   :depth: 3
   :class: new

Introduction
============

To use the statistical comparison features of |guppy|, it's a good idea to have a basic understanding of what the Kantorovich-Rubinstein (KR, a.k.a. earth-mover's distance) is doing, and how the edge PCA and squash clustering algorithms work. 
There is a gentle introduction in MatsenEvans2011_, and a more full treatment in EvansMatsen2010_.

Here's a table to demonstrate the relation of |guppy| concepts to ones which may be more familiar to the reader:

========================  =============
familiar concept          guppy concept 
========================  =============
weighted UniFrac          |KR| distance (kr_)
unweighted UniFrac        Phylogenetic diversity (pdfrac_)
OTU alpha diversity       the unary pairwise distance for each place file (uavgdist_)
OTU count                 PD of the subtree spanned by the placements (pd_)
UPGMA using UniFrac       "squash" clustering (cluster_)
PCA using UniFrac         Edge PCA (pca_)
========================  =============

The heat tree (heat_) and barycenter (bary_) have no analogs in previous types of phylogenetic microbial analysis.
Note that this table does not show strict equivalences, but rather a list of hints for further exploration.


Usage
=====

|guppy| does lots of different things-- it makes heat trees, makes matrices of Kantorovich-Rubinstein distances, does its own version of PCA, etc.
Each of these have their own options.
Rather than make a suite of little programs, we have opted for an interface analogous to git and svn: namely, a collection of different actions wrapped up into a single interface.

There are two ways to access the commands-- through the command line interface, and through the batch mode.
A list of these programs is below, and can always be found using ``guppy --cmds``


Command line interface
----------------------
The general way to invoke |guppy| is ``guppy COMMAND [options] placefile[s]`` where COMMAND is one of the |guppy| commands.
For example
```console
guppy heat --gray-black coastal.place DCM.place
```

These programs are described in more detail below (Commands_), and can always be found using ``guppy --cmds``.


Batch mode
----------
It's easy to run lots of commands at once with batch mode.
However, unlike running the equivalent set of commands on the command line, *.place files are only loaded once per batch file run*.
You don't have to specify loading them or anything; |guppy| just loads a given file the first time it is used in a command.

To use the batch mode, just put the commands, options, and placefiles you want into a file.
One line per command, and it's not necessary to write ``guppy COMMAND``. 
So the equivalent command to the above run in a batch file would be 
```console
heat --gray-black coastal.place DCM.place
```
Note that you need to specify each option each time you run a command-- they don't carry between lines of a batch file. 
That's on purpose.


phyloXML viewing notes
----------------------
|guppy| makes fattened and annotated trees to visualize the results of various analyses.
We have chosen to use phyloXML as the format for these trees, as it has width and color tags for edges; if you see .xml files coming out of a guppy analysis that's what they are for.
We like looking at these trees using the tree viewer archaeopteryx_.
If you open archaeopteryx with the default settings, you will see *nothing interesting*, simply the reference tree.
You need to click on the "Colorize Branches" and "Use branch-width" check boxes.
If you don't see those check boxes, then you need to edit the archaeopteryx configuration file so they do.

List of subcommands
===================

.. toctree::
   :maxdepth: 2
   :glob:

   generated_rst/*


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`



.. |guppy| replace:: ``guppy``
.. |pplacer| replace:: ``pplacer``
.. |KR| replace:: Kantorovich-Rubinstein

.. _pplacer: http://matsen.fhrcrc.org/pplacer
.. _phyloxml: http://phyloxml.org/
.. _archaeopteryx: http://www.phylosoft.org/archaeopteryx/
.. _EvansMatsen2010: http://arxiv.org/abs/1005.1699
.. _MatsenEvans2011: http://arxiv.org/abs/1005.1699

.. vim:set ai fo+=n fo-=l ft=rst:


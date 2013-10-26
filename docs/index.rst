pplacer documentation home
==========================

The pplacer suite consists of three separate binaries: pplacer, guppy, and rppr.
The pplacer binary actually does phylogenetic placement and produces place files, guppy does all of the downstream analysis of placements, and rppr does useful things having to do with reference packages.
If this is your first time looking at this documentation we suggest looking at our `overview`_.

Precompiled binaries for Mac OS X and Linux can be found on the `main pplacer site`_.

.. toctree::
   :maxdepth: 2

   generated_rst/pplacer
   generated_rst/guppy
   generated_rst/rppr
   scripts
   glossary
   compiling


.. raw:: html

   <h2> <font color="red">Warning</font> </h2>

pplacer is under heavy development, and not all parts are at the same level of maturity.
The general guideline is: if we have released a paper about a method, then we have verified and tested it.
Thus, the following functionality is stable:

* placement and visualization ability as described in the BMC Bioinformatics paper
* Kantorovich-Rubenstein distances
* edge principal components and squash clustering
* taxonomic/phylogenetic discordance analysis

We suggest that you wait to do any serious analysis using features not described on this list.
In particular, the taxonomic classification functionality is not stable and is still being validated.


Tutorial
--------

We have made a tutorial_ available that demonstrates most of the functionality of the pplacer suite of programs.
It can be downloaded and run as a shell script.


Getting help
------------

Your question might be covered in the pplacer FAQ already:

.. toctree::
   :maxdepth: 2

   faq

Otherwise, support for pplacer happens through the `user group`_.
If something is unclear in the manual and you can't find anything in past discussions, post to the group and we will get back to you right away.


Acknowledgements
----------------
The pplacer code and documentation is written and maintained by the Matsen group at the Fred Hutchinson Cancer Research Center.
Noah Hoffman has contributed many valuable ideas and bits of documentation.
We are greatly indebted to our biological collaborators, including
Ginger Armbrust,
David Fredricks,
Robin Kodner,
Martin Morgan,
and
Sujatha Srivnasan
for their suggestions.

.. _user group: http://groups.google.com/group/pplacer-users
.. _tutorial: http://fhcrc.github.com/microbiome-demo/
.. _main pplacer site: http://matsen.fhcrc.org/pplacer/
.. _overview: http://matsen.github.io/pplacer/generated_rst/pplacer.html#details

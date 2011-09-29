This subcommand outputs the classifications made by pplacer in a database or in a tabular format appropriate for use with R.

*The classifications made by the current implementation of pplacer are done with a simple, root-dependent algorithm.
We are currently working on improved algorithms.*
For best results, first taxonomically root the tree in your reference package (so that the root of the tree corresponds to the "deepest" evolutionary event according to the taxonomy).
This can be done automatically the `taxit reroot` command in taxtastic.
(Note that as of 27 May 2011, this requires the dev version of biopython available on github.)

The classifications are simply done by containment.
Say clade *A* of the reference tree is the smallest such that contains a given placement.
The most specific classification for that read will be the lowest common ancestor of the taxonomic classifications for the leaves of *A*.
If the desired classification is more specific than that, then we get a disconnect between the desired and the actual classification.
For example, if we try to classify at the species level and the clade LCA is a genus, then we will get a genus name.
If there is uncertainty in read placement, then there is uncertainty in classification.

For example, here is a classification list made for one read using the tabular output.
The columns are as follows: read name, attempted rank for classification, actual rank for classification, taxonomic identifier, and confidence.
You can see that in this example, there is some uncertainty at and below species, but only one classification at the genus level.

::

  GLKT0ZE01CQ2BU                      root          root       1          1
  GLKT0ZE01CQ2BU                below_root    below_root  131567          1
  GLKT0ZE01CQ2BU              superkingdom  superkingdom       2          1
  GLKT0ZE01CQ2BU        below_superkingdom  superkingdom       2          1
  GLKT0ZE01CQ2BU  below_below_superkingdom  superkingdom       2          1
  GLKT0ZE01CQ2BU               superphylum  superkingdom       2          1
  GLKT0ZE01CQ2BU                    phylum        phylum    1239          1
  GLKT0ZE01CQ2BU                 subphylum        phylum    1239          1
  GLKT0ZE01CQ2BU                     class         class  186801          1
  GLKT0ZE01CQ2BU                  subclass         class  186801          1
  GLKT0ZE01CQ2BU                     order         order  186802          1
  GLKT0ZE01CQ2BU               below_order         order  186802          1
  GLKT0ZE01CQ2BU         below_below_order         order  186802          1
  GLKT0ZE01CQ2BU                  suborder         order  186802          1
  GLKT0ZE01CQ2BU                    family        family  186804          1
  GLKT0ZE01CQ2BU              below_family        family  186804          1
  GLKT0ZE01CQ2BU                     genus         genus    1257          1
  GLKT0ZE01CQ2BU             species_group         genus    1257          1
  GLKT0ZE01CQ2BU          species_subgroup         genus    1257          1
  GLKT0ZE01CQ2BU                   species         genus    1257  0.0732247
  GLKT0ZE01CQ2BU                   species       species    1261   0.853561
  GLKT0ZE01CQ2BU                   species       species  341694   0.073214
  GLKT0ZE01CQ2BU             below_species         genus    1257  0.0732247
  GLKT0ZE01CQ2BU             below_species       species    1261   0.853561
  GLKT0ZE01CQ2BU             below_species       species  341694   0.073214

Sqlite
======

``guppy classify`` can also write its output into a sqlite3 database. The
argument to the ``--sqlite`` flag is the sqlite3 database into which the
results should be put. This database must have first been intialized using
``rppr taxtable``.

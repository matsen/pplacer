
``convexify`` applies an exact dynamic program to identify leaves of a phylogenetic tree that don't agree with their taxonomic labels.
You can read more in the announcement_ or the paper_.

You can either specify a reference package or a tree and a CSV file of colors.
The CSV file is formatted as follows::

  leaf_name1,color1
  leaf_name2,color2

where the colors are just strings.

Note that ``--no-early`` and ``--naive`` don't change the results.
They just (much) run more slowly for all but the most trivial problems.


.. _announcement: http://matsen.fhcrc.org/general/2011/09/27/convexify.html
.. _paper: http://arxiv.org/abs/1109.5423

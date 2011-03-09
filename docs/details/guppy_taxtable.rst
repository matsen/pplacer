Creates two SQL tables: ``taxa`` and ``ranks``.
``taxa`` maps between taxon IDs, taxon names, and rank names.
``rank`` maps between rank names and rank levels.

The databases made from these tables can then be used in conjunction with ``guppy classify --sqlite``.

See the `microbiome demo`_ for some examples.

.. _microbiome demo: http://fhcrc.github.com/microbiome-demo/

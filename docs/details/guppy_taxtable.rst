Creates and populates tables in the specified sqlite3 database, initializing
the database if it doesn't already exist. All of the tables created by this
command are:

* ``ranks`` -- all of the ranks contained in the provided reference package.
* ``taxa`` -- all of the taxa in the provided reference package.
* ``placements``, ``placement_names``, ``placement_classifications`` -- empty
  tables for ``guppy classify`` to insert data into.

In addition to the tables above, a view is also created:

* ``best_classifications`` -- for each placement, the ``tax_id``, ``rank``, and
  ``likelihood`` which represent the best classification. The best
  classification is the classification with the most specific rank and highest
  likelihood, where ``rank = desired_rank``.

See the `microbiome demo`_ for some examples of using ``guppy taxtable`` and
``guppy classify`` together.

.. _microbiome demo: http://fhcrc.github.com/microbiome-demo/

Creates and populates tables in the specified sqlite3 database, initializing
the database if it doesn't already exist. All of the tables created by this
command are:

* ``ranks`` -- all of the ranks contained in the provided reference package.
* ``taxa`` -- all of the taxa in the provided reference package.
* ``placements``, ``placement_names``, ``placement_classifications`` -- empty
  tables for ``guppy classify`` to insert data into.

See the `microbiome demo`_ for some examples of using ``guppy taxtable`` and
``guppy classify`` together.

.. _microbiome demo: http://fhcrc.github.com/microbiome-demo/

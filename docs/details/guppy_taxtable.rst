Creates and populates tables in the specified sqlite3 database, initializing
the database if it doesn't already exist. All of the tables created by this
command are:

* ``params`` -- parameters for some of the views discussed later.
* ``ranks`` -- all of the ranks contained in the provided reference package.
* ``taxa`` -- all of the taxa in the provided reference package.
* ``placements``, ``placement_names``, ``placement_classifications``,
  ``placement_positions`` -- empty tables for ``guppy classify`` to insert data
  into.

In addition to the tables above, some views are also created:

* ``best_classifications`` -- for each placement, the ``tax_id``, ``rank``, and
  ``likelihood`` which represent the best classification. The best
  classification is the classification with the most specific rank and highest
  likelihood, where ``rank = desired_rank`` and the likelihood is greater than
  the ``likelihood_cutoff`` parameter. If there is no such best classification,
  all three columns will be NULL.
* ``multiclass`` -- for each placement, one or more of the best

See the `microbiome demo`_ for some examples of using ``guppy taxtable`` and
``guppy classify`` together.

Example tables
--------------

The following shows examples of what the aformentioned tables could contain, as
well as the column names for each table. These examples are incomplete; real
data would contain many more rows.

``params``
~~~~~~~~~~

====================== =====
name                   val
====================== =====
likelihood_cutoff      0.9
multiclass_count       3.0
multiclass_likelihood  0.05
====================== =====

``ranks``
~~~~~~~~~

==========  ==========
rank        rank_order
==========  ==========
root        0
below_root  1
...
==========  ==========

``taxa``
~~~~~~~~

======  ==============================  =======
tax_id  tax_name                        rank
======  ==============================  =======
1       root                            root
103621  Actinomyces urogenitalis        species
109790  Lactobacillus jensenii          species
113286  Pseudoramibacter                genus
...
======  ==============================  =======

``placements``
~~~~~~~~~~~~~~

============ =
placement_id
============ =
1
2
3
...
============ =

``placement_names``
~~~~~~~~~~~~~~~~~~~

============  ==============  =======
placement_id  name            origin
============  ==============  =======
1             GLKT0ZE01B8WZE  p4z1r36
2             GLKT0ZE01CUQ8G  p4z1r36
3             GLKT0ZE01AIA4X  p4z1r36
3             GLKT0ZE01AT1UK  p4z1r36
3             GLKT0ZE01B8JSY  p4z1r36
3             GLKT0ZE01BG9QQ  p4z1r36
3             GLKT0ZE01EF2DE  p4z1r36
...
============  ==============  =======

``placement_classifications``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

============  =========================  ===============  ======  ===========
placement_id  desired_rank               rank             tax_id  likelihood
============  =========================  ===============  ======  ===========
117           family                     family           203492  0.999998
117           below_family               family           203492  0.999998
117           genus                      genus            168808  0.999998
117           species_group              genus            168808  0.999998
117           species_subgroup           genus            168808  0.999998
117           species                    genus            168808  0.77507
117           species                    species          187101  0.224928
117           below_species              genus            168808  0.77507
117           below_species              species          187101  0.224928
...
============  =========================  ===============  ======  ===========

``placement_positions``
~~~~~~~~~~~~~~~~~~~~~~~

This table's columns are abridged because it contains too many to briefly list.

============ ======== ======== ========= ========== ======
placement_id location ml_ratio distal_bl pendant_bl tax_id
============ ======== ======== ========= ========== ======
9            311      0.977324 0.528956  0.848439   836
9            331      0.011454 0.00383   1.146891   171549
9            330      0.011222 0.175601  1.152479   836
...
============ ======== ======== ========= ========== ======

``best_classifications``
~~~~~~~~~~~~~~~~~~~~~~~~

============ ====== ======== ==========
placement_id tax_id rank     likelihood
============ ====== ======== ==========
161          168808 genus    1.0
162          186802 order    1.000001
163          147802 species  0.999999
...
============ ====== ======== ==========

``multiclass``
~~~~~~~~~~~~~~

============ ====== ======== ==========
placement_id tax_id rank     likelihood
============ ====== ======== ==========
161          187101 species  0.857165
162          186803 family   0.705561
162          186806 family   0.29444
163          147802 species  0.999999
...
============ ====== ======== ==========


.. _microbiome demo: http://fhcrc.github.com/microbiome-demo/

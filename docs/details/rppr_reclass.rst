Suggest better classifications of sequences in a reference package.

``rppr reclass`` first looks at all ranks of a reference package, starting at
the most specific rank, until it finds the first rank with discordance. The
sequences corresponding to the cut leaves are then placed with pplacer against
the remaining sequences in the reference package.

The result is a table of the suggested reclassifications of these sequences:

============ ===========
Column name  Description
============ ===========
seq_name     The name of the sequence.
old_name     The name of the tax_id this sequence used to have.
old_taxid    The actual tax_id this sequence used to have.
new_name     The name of the suggested tax_id for this sequence.
new_taxid    The actual suggested tax_id.
makes_convex ``true`` if reclassifying this sequence with the new tax_id will result in a convex tree, otherwise ``false``.
old_avg_dist The average distance from this leaf to the convex subset of leaves classified with the previous tax_id.
new_avg_dist The average distance from this leaf to the convex subset of leaves classified with the suggested tax_id. [#f1]_
n_with_old   The number of leaves in the original tree which were classified with the previoux tax_id.
n_nonconvex  The number of leaves in the original tree classified with the previous tax_id which were also non-convex.
============ ===========

.. [#f1] This may be ``-`` if there are no leaves with that tax_id.

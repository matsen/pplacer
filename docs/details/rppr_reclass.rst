Suggest better classifications of sequences in a reference package.

``rppr reclass`` first looks at all ranks of a reference package, starting at
the most specific rank, until it finds the first rank with discordance.
Discordant leaves are cut off using the convexify algorithm. The sequences
corresponding to the cut leaves are then placed with pplacer against the
remaining sequences in the reference package.

The result is a table of the suggested reclassifications of these sequences:

=============== ===========
Column name     Description
=============== ===========
seq_name        The name of the sequence.
old_name        The name of the tax_id this sequence used to have.
old_taxid       The actual tax_id this sequence used to have.
new_name        The name of the suggested tax_id for this sequence.
new_taxid       The actual suggested tax_id.
makes_convex    ``true`` if reclassifying this sequence with the new tax_id will result in a convex tree, otherwise ``false``.
uninformative   ``true`` if this sequence sits inside a clade determined to be uninformative, otherwise ``false``.
old_median_dist The median distance from this leaf to the convex subset of leaves classified with the previous tax_id.
old_avg_cv      The coefficient of variation of the distances from this leaf to the convex subset of leaves classified with the previous tax_id, multiplied by 100%. [#f1]_
new_median_dist The median distance from this leaf to the convex subset of leaves classified with the suggested tax_id. [#f2]_
new_avg_cv      The coefficient of variation of the distances from this leaf to the convex subset of leaves classified with the suggested tax_id, multiplied by 100%. [#f1]_
n_with_old      The number of leaves in the original tree which were classified with the previoux tax_id.
n_nonconvex     The number of leaves in the original tree classified with the previous tax_id which were also non-convex.
=============== ===========

.. [#f1] This may be ``-`` if there are only zero or one other leaves with that tax_id.
.. [#f2] This may be ``-`` if there are no other leaves with that tax_id.

Uninformative clades are determined to be clades containing all of the
representatives of exactly two tax_ids which are not found anywhere else in the
tree.

The suggestion tree emitted with the ``-t`` flag is similar to the discordance
tree emitted by ``rppr convexify``; it colors all of the leaves which were cut
by the convexify step red, colors all of the edges which are considered
uninformative (with the overlap between these two sets colored orange), and
relabels the cut sequences to include the name of the suggested new
classification. The new label is in the format ``seq_name -> new_name``, using
the column names described above.

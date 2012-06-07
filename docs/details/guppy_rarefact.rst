Calculate phylogenetic rarefaction curves.

For every :math:`k \in [2, n]`, where n is the number of pqueries in a
placefile, subsample the given placefile to contain every combination of
:math:`k` pqueries and calculate the mean and variance of phylogenetic divesity
for all of these subsampled placefiles.

The ``rooted_mean`` and ``rooted_variance`` columns are respectively the mean
and variance of the phylogenetic diversity only requiring mass on the distal
side of the edge (which normally requires mass proximal and distal to the
edge).

*Experimental.*

# COMMIESIM


## Data structures

* split: bipartition (just a set of leaves if we are careful)
* lset: set of leaves (representing a connected subset)
* lsetset: set of lset (representing a collection of connected subsets
  for a given community)
* map from internal nodes to lsetset

## Functions that will be needed

### Generalitites

* `uniform_sample: set -> int -> set`
  * uniformly draws a subset of the given size from the given set
* Don't forget about `Set.inter` and `Set.diff`, etc

### Tree related things

* `get_sset: tree -> split set`
  * return the sset for a given tree
* `generate_yule: Gsl_rng.t -> int -> stree`
  * makes a yule/coalescent tree. Should the rng be an explicit
    argument? Otherwise we are up to the vagaries of Random.
    This should generate names for the leaves of the tree.

### Concerning lsetset

* `split_lset: split -> lset -> lsetset`
  * If sigma does not split X, then `split_lset(sigma, X)` returns the
    lsetset containing only X. Otherwise say sigma = U|V, in which case
    it returns the lsetset consisting of X intersect U and X intersect
    V.
* `split_lsetset: split -> lsetset -> lsetset`
  * `split_lsetset(sigma, A)` will give the union of all of the lsetsets
    obtained by applying sigma to each of the lsets in A.
* `sset_lsetset: sset -> lsetset -> lsetset`
  * multiple application of `split_lsetset`. Take union at end.
* `split_does_cut_lset: split -> lset -> bool`
  * does this split cut a given lset?
* `split_does_cut_lsetset: split -> lsetset -> bool`
  * does this split cut one of the lset in the lsetset?
* `select_sset_cutting_lsetset: sset -> lsetset -> sset`
  * get the subset of splits that actually cut the given lsetset
* `generate_root: include_prob:float -> poisson_mean:float -> sset ->
  lsetset`
  * This will make poisson-distributed cuts in the tree and then select
    them to include with the given probability.
* `uniform_partition: lsetset -> int -> lsetset list`
  * throw each lset in a lsetset uniformly into k lsetset such that each
    lsetset has at least one lset. Can do this by first uniformly
    selecting k lset from lsetset for the list, then adding other lset
    uniformly in a second stage.
* `generate_placements: lsetset -> pquery list`
  * Sample locations for placements with a balls-in-boxes approach


## "Main"

* input parameters needed:
  * either a tree is given or a desired number of leaves for a Yule tree
  * `include_prob`
  * `n_reads`
  * `poisson_mean`
  * destination directory
* Start with a lsetset made by `generate_root`
* let `cluster_tree` be the tree on which we are simulating
* Make a map from the internal nodes of `cluster_tree` to lsetset
  recursively.
  * Say we are at internal node a, and that b1,...,bk are the
    descendants. Say A is the lsetset associated with a. We are given the
    set of splits (called sigma) that cut A.
  * Sample a poisson distribution truncated at the size of sigma to get
    an integer k.
  * Sample k splits from sigma
  * Apply these using `sset_lsetset` to the lsetset associated with a,
    then `uniform_partition` to distribute them to b1,...,bk.
  * Figure out the splits that are associated with each subtree and
    recur on descendants
* Let `represented_lset` be the union of all leaves in
  `complete_lsetset`
* Throw down "reads" uniformly on the leaves, making a map from leaves
  to ints (the number of reads). Can do this using multinomial
  distribution in GSL. Or just increment an array, by sampling uniformly.
* Turn each of the lsetset at the tips of `cluster_tree` into a placerun
  by attaching the designated number of reads to each leaf in the
  lsetset.
* Write them out according to the name of the leaf.


## interface

* Command line arguments for necessary parameters


## Future directions

* define `place_type: Leaf | Internal` and allow sampling of internal
  nodes when attaching edges for `generate_placements`

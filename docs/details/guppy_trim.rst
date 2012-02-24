This subcommand is a quick root-dependent way to trim the reference tree to what is relevant for a collection of placements.

The first step is to select a collection of leaves that will be present in the trimmed tree.
Convert all of the placements into mass by unitizing mass for each placerun individually and then take the overall average of those collections of masses.
Then select any leaf for inclusion that has at least ``--min-path-mass`` on the path from the root to the leaves.

Once those leaves are selected, we take the induced subtree on those leaves. That is, if we were to take the induced subtree of

::

    ^
   / \
  /\  \
  ab  /\
     c d

with the leaf set {a,d}, we would get the tree

::

    ^
   / \
  /   \
  a    \
       d

with the branch lengths induced by adding branch lengths along edges that are not bifurcating.

This trimmed tree is then put in the placefile.
Note that even though the tree is different than the one in the reference package, we have arranged things so that it's possible to use the same reference package with a trimmed tree.

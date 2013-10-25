Perform length principal components analysis ("length PCA").
Length PCA takes the special structure of phylogenetic placement data into account.
Consequently, it is possible to visualize the principal component eigenvectors, and it can find consistent differences between samples which may not be so far apart in the tree.
Length PCA is similar to :doc:`guppy_epca` but is invariant to edge subdivision on the reference tree where edge PCA is invariant to branch length.
Running this command produces the following files for a run with out prefix set to ``out``:

out.trans
  The top eigenvalues (first column) then their corresponding eigenvectors.

out.proj
  The samples projected into principal coordinate space.

out.xml
  The eigenvectors visualized as fattened and colored trees.

The ``--som`` flag triggers a Support Overlap Minimization (SOM) rotation of the principal components. Setting this value to ``n`` triggers a rotation of the first n principal component vectors such that the overlap in support (non-zero vector entries) between the vectors is minimized. This can make the projections easier to interpret from a biological perspective, but care should be taken not to rotate noise into more meaningful components (a good rule of thumb is not to rotate vectors with less than 10% of the variance).

Acceptable values are 0 (no rotation; the default), 2 or 3. Looking at the output from the non-rotated principal components can help you determine what is most appropriate here. If 2 or 3 are specified, the follows files will also be output:

out.som
  The rotated eigenvectors and corresponding variance values.

out.som.xml
  The rotated vectors visualized as fattened and colored trees.


See the :doc:`guppy_splitify` documentation for information about the ``--kappa`` flag.

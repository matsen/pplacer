Perform edge principal components analysis ("edge PCA").
Edge PCA takes the special structure of phylogenetic placement data into account.
Consequently, it is possible to visualize_ the principal component eigenvectors, and it can find consistent differences between samples which may not be so far apart in the tree.

Running this command produces the following files for a run with out prefix set to ``out``:

out.rot
  The top eigenvalues (first column) then their corresponding eigenvectors.

out.trans
  The samples projected into principal coordinate space.

out.xml
  The eigenvectors visualized as fattened and colored trees.

.. _visualize: http://matsen.fhcrc.org/pplacer/demo/pca.html

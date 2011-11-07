
The first step to perform edge PCA is to make a matrix with rows indexed by the samples, and columns by the edges of the tree.
The :math:`(s,e)` entry of this matrix is the difference between the distribution of mass on either side of edge :math:`e` for the sample :math:`s`.
Specifically, it is the amount of mass on the distal (non-root) side of edge :math:`e` minus the amount of mass on the proximal side.
The matrix is indexed such that the first numerical column is edge labeled 0 in the reference tree.
The ``splitify`` subcommand simply writes out this matrix.

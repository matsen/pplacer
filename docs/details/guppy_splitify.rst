
The first step to perform edge PCA is to make a matrix with rows indexed by the samples, and columns by the edges of the tree.
The :math:`(s,e)` entry of this matrix is the difference between the distribution of mass on either side of edge :math:`e` for the sample :math:`s`.
Specifically, it is the amount of mass on the distal (non-root) side of edge :math:`e` minus the amount of mass on the proximal side.
The matrix is indexed such that the first numerical column is edge labeled 0 in the reference tree.
The ``splitify`` subcommand simply writes out this matrix.

Specifying ``--rep-edges x`` will only take representatives from collections of neighboring edges whose Euclidean distance between splitified columns is less than ``x``.

The ``--kappa`` option enables a componentwise transformation :math:`\varphi_\kappa` on the entries of this matrix.

.. math::
  \varphi_\kappa(x) = \mathrm{sgn}(x) |x|^\kappa

where the :math:`\kappa` parameter can any non-negative number.
This parameter scales between ignoring abundance information (:math:`\kappa = 0`), using it linearly (:math:`\kappa = 1`), and emphasizing it (:math:`\kappa > 1`).



This is the power tool for splitting apart place files and applying mass
transforms.

If either the ``--transform`` or ``--unitize`` flags are specified, then the
resulting placefiles will have mass instead of a name list. Otherwise, the
output placefiles will have either mass or a name list depending on what the
input placefiles had.

To do nothing but transform the input placefiles to placefiles with mass
instead of a name list, specify ``--transform no_trans``.

One can use the ``--leaf-values`` flag to correct for copy number. This flag
runs the internals of :doc:`guppy_indep_c` and then divides the masses of the
placements by the inferred value, which can be copy number if you supply
``--leaf-values`` with a CSV file of copy numbers for reference sequences.


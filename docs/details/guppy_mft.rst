
This is the power tool for splitting apart place files and applying mass
transforms.

If either the ``--transform`` or ``--unitize`` flags are specified, then the
resulting placefiles will have mass instead of a name list. Otherwise, the
output placefiles will have either mass or a name list depending on what the
input placefiles had.

To do nothing but transform the input placefiles to placefiles with mass
instead of a name list, specify ``--transform no_trans``.

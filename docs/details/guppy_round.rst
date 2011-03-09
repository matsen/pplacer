This subcommand provides very basic "clustering" functionality.
This is not meant to derive "species level" or analogous groupings, but rather to group placements which will have essentially identical impact in a guppy analysis.

Rounding is done as follows:

  * throw out the placement mass below the ``--cutoff`` level
  * round the branch lengths to ``--sig-figs`` significant figures
  * group placements that are identical after this process.

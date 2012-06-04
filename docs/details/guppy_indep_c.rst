This is an implementation of the "independent contrasts" method of `(Felsenstein, 1985)`_.

Assume we have the following pair of placements on a simple tree::

  {"tree": "((A:2{0},B:9{1}):7{2},C:5{3},D:1{4}):0{5};", "placements":
    [{"p": [[0, -1309.83, 1, 1, 10]], "n": ["read1"]},
      {"p": [[1, -1309.83, 1, 2, 10]], "n": ["read2"]}
    ], "version": 3, "metadata": {}, "fields":
    ["edge_num", "likelihood", "like_weight_ratio", "distal_length",
      "pendant_length"
    ]
  }

And that ``ic.csv`` is a (possibly partial) list of tip values as follows::

  A,3
  B,0
  D,1

Then, a run of ``indep_c`` will give you the maximum likelihood values of
values at the tips assuming that the value changes by Brownian motion::

  rhino2 ~/ic » guppy indep_c --leaf-values ic.csv ex.jplace
  sequence         x
     read1   2.60377
     read2  0.490566

Note that :doc:`guppy_mft` has the ``--leaf-values`` option to divide the placement weights by these inferred values.

.. _(Felsenstein, 1985): http://www.jstor.org/discover/10.2307/2461605

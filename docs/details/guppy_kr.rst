
``kr`` calculates the Kantorovich-Rubinstein distance between collections of placements (given by their place files) by its closed form formula

.. math::
    Z(P,Q) =
    \int_T \left| P(\tau(y)) - Q(\tau(y)) \right| \, \lambda(dy).

This is a generalization of the UniFrac distance (UniFrac can only place mass at leaves and cannot accomodate uncertainty).
There is a further generalization to an :math:`L^p` Zolotarev-type version:
for :math:`0 < p < \infty` we have the distances

.. math::

    Z_p(P,Q) =
    \left[\int_T \left| P(\tau(y)) - Q(\tau(y)) \right|^p \, \lambda(dy)\right]^{\frac{1}{p} \wedge 1}

which can be used to vary the impact of mass relative to transport.
A larger :math:`p` increases the impact of differences of mass, while a smaller :math:`p` emphasizes distance traveled.

Note that the significance p-values calculated by ``-s`` or ``--gaussian`` are not corrected for multiple comparison.

See `Evans and Matsen`_ for more details.

.. _Evans and Matsen: http://arxiv.org/abs/1005.1699


`kr` calculates the Katorovich-Rubinstein distance between collections of placements (given by their place files).

.. math::

    Z(P,Q) : = \inf\left\{ \int_{S \times S} r(x,y) \, R(dx,dy) : R \in \mathcal{R}(P,Q) \right\}

by its closed form formula

.. math::
    Z(P,Q) = 
    \int_T \left| P(\tau(y)) - Q(\tau(y)) \right| \, \lambda(dy).

This generalizes to an :math:`L^p` Zolotarev-type version:
for :math:`0 < p < \infty` we have the distances

.. math::

    Z_p(P,Q) = 
    \left[\int_T \left| P(\tau(y)) - Q(\tau(y)) \right|^p \, \lambda(dy)\right]^{\frac{1}{p} \wedge 1}


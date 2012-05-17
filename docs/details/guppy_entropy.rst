With :math:`\ell_e` as the branch length of an edge snippet and :math:`p_e` as
the total mass on one side of the snippet, calculate phylogenetic entropy as
described in `Allen, Kon, and Bar-Yam`_:

.. math:: - \sum_e \ell_e \, p_e \log p_e

and quadratic entropy as described in `Warwick and Clarke`_:

.. math:: \sum_e \ell_e \, p_e ( 1 - p_e ).

.. _Allen, Kon, and Bar-Yam: http://www.people.fas.harvard.edu/~ballen/NewPhylogenetic.pdf
.. _Warwick and Clarke: http://www.int-res.com/articles/meps/129/m129p301.pdf


The expected distance between placement locations (EDPL) is a means of understanding the uncertainty of a placement using pplacer.
The motivation for using such a metric comes from when there are a number of closely-related sequences present in the reference alignment.
In this case, there may be considerable uncertainty about which edge is best as measured by posterior probability or likelihood weight ratio.
However, the actual uncertainty as to the best region of the tree for that query sequence may be quite small.
For instance, we may have a number of very similar subspecies of a given species in the alignment, and although it may not be possible to be sure to match a given query to a subspecies, one might be quite sure that it is one of them.

The EDPL metric is one way of resolving this problem by considering the distances between the possible placements for a given query.
It works as follows.
Say the query bounces around to the different placement positions according to their posterior probability; i.e. the query lands with location one with probability :math:`p_1`, location two with probability :math:`p_2`, and so on.
Then the EDPL value is simply the expected distance it will travel in one of those bounces (if you don't like probabilistic language, it's simply the average distance it will travel per bounce when allowed to bounce between the placements for a long time with their assigned probabilities).
Here's an example, with three hypothetical locations for a given query sequence:

.. image:: ../_static/edpl_formula.png




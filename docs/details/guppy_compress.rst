A cutoff *c* is specified via a command line flag.
The `compress` command merges pairs of pqueries that have KR distance between them less than *c*.

To compress the pqueries:

* divide the pqueries into sets via :doc:`guppy_islands` or :doc:`guppy_mcl`

* for each pquery set, calculate all of the pairwise distances between the pqueries for that island and put them in a matrix
* build a graph such that the nodes are pqueries, and there is an edge between them if their distance is less than *c*
* merge pqueries according to this graph as described below.


Each original pquery (=node) will get merged into one of the the selected pqueries.
This will happen as follows.
Maintain a set of unmerged pqueries, and a set of pairs *(w, d(w))*, where *w* is a selected pquery and *d(w)* is the degree of *w* in the graph.

* find the *(w, d(w))* pair with the greatest d(w) and remove it from the set
* find all of the unmerged pqueries that are adjacent to *w* in the graph, and merge their mass into *w*. Remove *w* and all of the adjacent pqueries from the unmerged pquery set.
* repeat!

Stop when the unmerged pquery set is empty.


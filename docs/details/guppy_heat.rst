Makes

For example, given a tree like so::

((A:2{0},B:9{1}):7{2},C:5{3},D:1{4}):0{5};

and a CSV line::

"test",0,3,0,0,-6,1

this command run with a total width of 300 will put width 90 in red on the edge leading to B, width 180 in blue on the edge leading to D, and width 30 in red on the root edge.


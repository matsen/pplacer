#!/usr/bin/env python
# this script prints every tree in "flattened" form, i.e. for every clade it writes out the clade name, the boot support, and the list of children
# we assume that every one of those is supplied.

import sys
from Bio import Phylo

def print_flat_tree(tree_fname):
    tree = Phylo.read(tree_fname,'phyloxml')
    for clade in tree.find_clades():
        if len(clade) > 0:
            print ",".join([clade.name, str(clade.confidences[0].value)] + 
                           [term.name for term in clade.get_terminals()])

for tree_fname in sys.argv[1:]:
    print "# "+tree_fname
    print "name,boot"
    print_flat_tree(tree_fname)
    print ""

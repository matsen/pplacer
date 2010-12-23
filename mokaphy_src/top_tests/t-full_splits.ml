open Full_splits
open MapsSets
#install_printer ppr;;

let t = Newick.of_file "top_tests/smallish.tre"
let s = Gtree.get_stree t
let tp = IntMapFuns.to_pairs

let d = make_distal_map s
let x = tp d
let p = make_proximal_map d s
let x = tp p
let f = full_splits s
let x = tp f



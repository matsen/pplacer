open Edge_rdist

let quartet = Newick_gtree.of_string "(((A:1,B:1):1,C:1):10,D:1):100;";;
let t = Newick_gtree.of_string "((A:1,B:1):0.1,((C:1,D:1):0.3,(E:1,(F:1,G:1):0.7):0.6):0.2):0.5;";;

let st = Gtree.get_stree t;;

let dmq = build_pairwise_dist quartet;;
let dm = build_pairwise_dist t;;

let u = build_ca_info quartet;;
let u = build_ca_info t;;

let d = find_ca_dist u (6, 0.) (8, 0.)
let d = find_ca_dist u (10, 0.3) (8, 0.9)



let test_rt = Newick_gtree.of_string "((A:2,B:9):7,C:5,D:1):0;";;
let u = build_ca_info test_rt;;

let d = find_ca_dist u (1,7.) (2,2.)

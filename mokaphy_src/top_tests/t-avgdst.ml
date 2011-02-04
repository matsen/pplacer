open MapsSets
open Avgdst


let p1 = Placerun_io.of_file "top_tests/test1.place"
let p2 = Placerun_io.of_file "top_tests/test2.place"
let p3 = Placerun_io.of_file "top_tests/test3.place"

let prl = [p1; p2; p3]

let t = Cmds.list_get_same_tree prl
let tl = Gtree.tree_length t

let dist_fun = 
  Pquery_distances.dist_fun_of_w 
    Mass_map.Weighted
    Placement.ml_ratio
    (Edge_rdist.build_ca_info t)

let udists = List.map (of_placerun dist_fun 1.) prl

let timestree = List.map (( *. ) tl) udists

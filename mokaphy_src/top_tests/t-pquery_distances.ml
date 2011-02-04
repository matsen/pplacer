open Induced
open MapsSets

let i_of_prf fname = of_placerun Placement.ml_ratio (Placerun_io.of_file fname)
let ia = i_of_prf "top_tests/test_all.place"
let t = Placerun.get_ref_tree (Placerun_io.of_file "top_tests/test_all.place")
let s = Gtree.get_stree t
let test f pl1 pl2 = f t (IntMapFuns.of_pairlist pl1) (IntMapFuns.of_pairlist pl2)

let () = print_endline "intersections"
let at_mrca = test intersect [0, 0.5;] [1, 0.5;]
let on_same_edge = test intersect [1, 1.;] [1, 3.;]
let on_serial_edges = test intersect [1, 1.;] [2, 1.0;]
let root_mrca = test intersect [1, 1.;] [3, 0.;]

let () = print_endline "unions"
let at_mrca = test union [0, 0.5;] [1, 0.5;]
let on_same_edge = test union [1, 1.;] [1, 3.;]
let on_serial_edges = test union [1, 1.;] [2, 1.0;]
let root_mrca = test union [0, 0.5; 1, 1.;] [3, 0.;]


open MapsSets
open Induced
open Pdfrac


let i_of_prf fname = of_placerun Placement.ml_ratio (Placerun_io.of_file fname)
let ia = i_of_prf "top_tests/test_all.place"
let t = Placerun.get_ref_tree (Placerun_io.of_file "top_tests/test_all.place")
let s = Gtree.get_stree t
let test pl1 pl2 = of_induceds t (IntMap.of_pairlist pl1) (IntMap.of_pairlist pl2)
(* with same calculations done by hand *)
let at_mrca = test [0, 1.;] [1, 1.;]
let x = 7. /. 16.
let on_same_edge = test [1, 1.;] [1, 3.;]
let x = 13. /. 15.
let on_serial_edges = test [1, 1.;] [2, 1.;]
let x = 6. /. 15.
let root_mrca = test [1, 1.;] [3, 0.;]

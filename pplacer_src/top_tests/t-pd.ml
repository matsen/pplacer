open Pd
open Ppatteries

(*
20/24 = .83333333333333333333
*)
let pr = Placerun_io.of_file "top_tests/test_all.place"
let i = Induced.of_placerun Placement.ml_ratio pr
let p = of_pr Placement.ml_ratio pr
let np = normalized_of_pr Placement.ml_ratio pr

let t = Placerun.get_ref_tree pr
let s = Gtree.get_stree t
let test pl = of_induced t (IntMap.of_pairlist pl)
let single_path = test [1, 3.]
let mrca = test [0, 1.; 1, 3.]

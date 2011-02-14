(*
let t = Gtree.get_stree (Newick.of_string "((a,b),(c,d))");;
let ids = Barycenter.collect_distal_ids t 2;;
let ids = Barycenter.collect_proximal_ids t 2;;

let l2 = find Mass_map.Weighted Placement.ml_ratio pr2
let l3 = find Mass_map.Weighted Placement.ml_ratio pr3

let pr2 = Placerun_io.of_file "test2.place"
let pr3 = Placerun_io.of_file "test3.place"
*)

#install_printer Fam_gsl_matvec.ppr_gsl_vector
#install_printer Fam_gsl_matvec.ppr_gsl_matrix
#install_printer Mass_map.Indiv.ppr
#install_printer Stree.ppr
#install_printer Newick.ppr


open Barycenter

let mop fname =
  Mass_map.Indiv.of_placerun Mass_map.Unweighted Placement.ml_ratio
  (Placerun_io.of_file fname)

let rt = Placerun.get_ref_tree (Placerun_io.of_file "test1.place")

let m0 = mop "test0.place"
let m1 = mop "test1.place"
let m2 = mop "test2.place"
let m3 = mop "test3.place"

let q0 = find 1. rt m0
let q1 = find 1. rt m1
let q2 = find 1. rt m2
let q3 = find 1. rt m3

(*
 *

 * let q4 = of_placerun Mass_map.Unweighted Placement.ml_ratio 1.
 * (Placerun_io.of_file "testy.place") *)

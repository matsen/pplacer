open Matrix_sig

let pr1 = Placerun_io.of_file "top_tests/test1.place";;
let pr3 = Placerun_io.of_file "top_tests/test3.place";;
let n1 = Placerun.n_pqueries pr1;;
let n3 = Placerun.n_pqueries pr3;;

let ca_info = Camat.build_ca_info (Placerun.get_same_tree pr1 pr3);;

let d = Camat.find_ca_dist ca_info (2,2.) (2,2.);;

let mt = build_mtilde Mass_map.Weighted Placement.ml_ratio pr1 pr3;;
let ra = row_avg mt;;

let x = m_of_mtilde mt n1 n3;;

mt;;

(*
let m = build_m false pr1 pr3;;


let p = matrix_pvalue rng false pr1 pr3;;
*)


let rng = Gsl_rng.make Gsl_rng.KNUTHRAN2002;;
Gsl_rng.set rng (Nativeint.of_int 1);;

let x = Gsl_randist.gaussian rng ~sigma:1.;;

let n1 = 3;;
let n2 = 2;;

let m = Fam_matrix.init 5 5 (fun i j -> if i=j then 1. else 0.)

let _ = m_of_mtilde m n1 n2;;

m;;

let ew = w_expectation rng 1e-5 m;;
let tw = Fam_matrix.trace m;;



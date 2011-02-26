open OUnit
open Test_util

open Diagd
open Fam_gsl_matvec

let b = Gsl_matrix.of_arrays [|[|-. 1.; 0.15|];[|0.15; -.2.|]|];;
let d = Gsl_vector.of_array [|0.25; 0.75|];;
let diagd = of_d_b d b;;

let test_dediag _ =
  let a = mm (diagOfVec d) b
  and a_alt = to_matrix diagd in
  "dediag not remaking the matrix" @? (a ^=^ a_alt)

let exp1 = Gsl_matrix.of_arrays
  [|
    [|0.77992929; 0.01668155|];
    [|0.05004464; 0.22387769|];
  |]
let exp4 = Gsl_matrix.of_arrays
  [|
    [|0.37187386; 0.011053085|];
    [|0.03315925; 0.003437709|];
  |]

let test_to_exp _ =
  "to_exp" @? ((to_exp diagd 1.) ^=^ exp1);
  "to_exp" @? ((to_exp diagd 4.) ^=^ exp4);
  ()

let test_multi_exp _ =
  let t = Tensor.create 2 2 2 in
  (* multiply to 1.; 4. *)
  multi_exp t diagd [|2.;8.|] 0.5;
  "exponentiating matrices" @?
    ((matrices_approximately_equal exp1 (Tensor.BA3.slice_left_2 t 0)) &&
    (matrices_approximately_equal exp4 (Tensor.BA3.slice_left_2 t 1)));
  ()


let noep = normed_of_exchangeable_pair b d;;
to_matrix noep;;
to_exp noep 1.;;



let suite = [
  "dediag" >:: test_dediag;
  "to_exp" >:: test_to_exp;
  "multi_exp" >:: test_multi_exp;
]

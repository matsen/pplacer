open OUnit
open Test_util
open Pca
open Linear_utils
open Power_iteration

let r_cov = mat_of_string
"14.250000 -2.333333 -10.91667
-2.333333  2.000000   3.00000
-10.916667  3.000000  10.91667";;

let eiga = top_eigs r_cov 1e-15 100 3;;
let iter_lv = Gsl_vector.of_array (Array.map (fun e -> e.l) eiga);;
let pre_iter_vm =
  Gsl_matrix.of_arrays (Array.map (fun e -> Gsl_vector.to_array e.v) eiga);;
(* Transpose so vectors are columns. *)
Gsl_matrix.transpose_in_place pre_iter_vm;;
(* Flip signs (equivalent answers). *)
let iter_vm =
  alloc_mat_mat_mul
    pre_iter_vm
    (diag (Gsl_vector.of_array [|-1.;-1.;1.;|]));;

let (lv, vm) = Gsl_eigen.symmv ~protect:true (`M r_cov);;

let test_eigen lv1 lv2 vm1 vm2 =
  "eigenvalues not equal" @? (lv1 =| lv2);
  "eigenvectors not equal" @? (vm1 =|| vm2);
  ()

let suite = [
  "r_cov" >:: (fun _ -> test_eigen lv iter_lv vm iter_vm)
]

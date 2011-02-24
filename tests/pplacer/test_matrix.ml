open OUnit
open Test_util
open Fam_gsl_matvec

let mm = allocMatMatMul;;

(* this checks to make sure that symmv is giving us the eigenvectors as
 * column vectors. *)
let check_symmv m =
  let (l,v) = Gsl_eigen.symmv (`M(m)) in
  let m' = mm v (mm (diagOfVec l) (alloc_transpose v)) in
  "symmEigs not giving column vectors" @? matrices_approximately_equal m m'

let test_symmv _ =
  let b = Gsl_matrix.of_arrays [|[|-. 1.; 0.15|];[|0.15; -.2.|]|] in
  check_symmv b

let suite = [
  "test_symmv" >:: test_symmv;
]

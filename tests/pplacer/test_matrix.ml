open OUnit
open Test_util
open Linear_utils

let mm = alloc_mat_mat_mul;;

(* this checks to make sure that symmv is giving us the eigenvectors as
 * column vectors. *)
let check_symmv m =
  let (l,v) = Gsl_eigen.symmv (`M(m)) in
  let m' = mm v (mm (diag l) (alloc_transpose v)) in
  "symmEigs not giving column vectors" @? mat_approx_equal m m'

let test_symmv _ =
  let b = Gsl_matrix.of_arrays [|[|-. 1.; 0.15|];[|0.15; -.2.|]|] in
  check_symmv b

let suite = [
  "test_symmv" >:: test_symmv;
]

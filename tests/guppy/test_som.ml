open Test_util
open Som
open OUnit


let test_rot_mat mat angles () =
  "rotation matrix does not agree with mathematica" @? mat_approx_equal mat (rot_mat angles)

let is_inverse mat1 mat2 =
  let id = mat_of_string "1.0 0.0 0.0
                          0.0 1.0 0.0
                          0.0 0.0 1.0"
  and res = Gsl_matrix.copy mat1 in
  Gsl_blas.gemm ~ta:Gsl_blas.NoTrans ~tb:Gsl_blas.NoTrans ~alpha:1.0 ~beta:0.0 ~a:mat1 ~b:mat2 ~c:res;
  mat_approx_equal res id

let test_rot_mat_orth angles () =
  let orig = rot_mat angles in
  let dim = fst (Gsl_matrix.dims orig) in
  let trans = Gsl_matrix.create dim dim in
  Gsl_matrix.transpose trans orig;
  "non orthogonality in rotation matrix" @? is_inverse orig trans

(* From R - for testing an actual som, both 2d and 3d *)
(* # Imaginary pca transform matrix - obviously, this has minimal support overlap
   # for the first three components.
   > trans
        [,1] [,2] [,3] [,4] [,5]
   [1,]    1    0    0    0    0
   [2,]    0    1    0    0    0
   [3,]    0    0    1    0    0
   [4,]    1    4    3    2   -8

   # ...so if we ask for a 2d rot som from the following matrix, then we should get
   # trans back....
   > rotateTrans(trans[1:3,], c(0.2, 0, 0))
              [,1]      [,2] [,3] [,4] [,5]
   [1,]  0.9800666 0.1986693    0    0    0
   [2,] -0.1986693 0.9800666    0    0    0
   [3,]  0.0000000 0.0000000    1    0    0

   # And similarly here for a 3d som
   > rotateTrans(trans[1:3,], c(0.2, 0.5, -0.81))
              [,1]      [,2]       [,3] [,4] [,5]
   [1,]  0.8600893 0.1743487 -0.4794255    0    0
   [2,] -0.4773022 0.6067681 -0.6356218    0    0
   [3,]  0.1800803 0.7755224  0.6050918    0    0
*)

let rot_angles = [|
  [| 1.2; -0.3; 3.56 |];
  [| 0.; 2.1; -1.1 |]
|]

let minimized_trans = farrarr_of_string
"1.0  0.0  0.0  0.0  0.0
 0.0  1.0  0.0  0.0  0.0
 0.0  0.0  1.0  0.0  0.0
 1.0  4.0  3.0  2.0 -8.0"

let mat_for_2d_som = farrarr_of_string
"0.9800666 0.1986693  0.0  0.0  0.0
-0.1986693 0.9800666  0.0  0.0  0.0
 0.0000000 0.0000000  1.0  0.0  0.0
 1.0       4.0        3.0  2.0 -8.0"

let mat_for_3d_som = farrarr_of_string
"0.802033    -0.485969   -0.347242 0.0  0.0
 0.589636    0.736924    0.330563  0.0  0.0
 0.0952472   -0.469869   0.877583  0.0  0.0
 1.0         4.0         3.0       2.0 -8.0"

let dummy_vars = farr_of_string "0.5 0.25 0.15 0.1"

let test_som_2d () =
  let _, vects = som_rotation mat_for_2d_som 2 dummy_vars in
  "2d som incorrect" @? farrarr_approx_equal minimized_trans vects

let test_som_3d () =
  let _, vects = som_rotation mat_for_3d_som 3 dummy_vars in
  Ppr.print_float_array_array vects;
  "3d som incorrect" @? farrarr_approx_equal minimized_trans vects

let test_som_3d_var_order orig_vars () =
  let vars, _ = som_rotation mat_for_3d_som 3 orig_vars
  and inv_compare x y = -1 * (compare x y) in
  let var_copy = Array.copy vars in
  Array.sort inv_compare var_copy;
  "var order changed but shouldn't" @? (vars = var_copy)

let suite = [
  "rot_mat test 1" >:: test_rot_mat
                          (mat_of_string
                           "0.921649 0.383557 0.0587108
                            -0.387517 0.902113 0.189796
                            0.0198338 -0.197677 0.980067")
                          [|0.1; 0.2; 0.3|];
  "rot_mat test 2" >:: test_rot_mat
                          (mat_of_string
                           "0.802033    -0.485969   -0.347242
                            0.589636    0.736924    0.330563
                            0.0952472   -0.469869   0.877583")
                           [|0.2; 0.5; -0.81|];
  "orth test 1" >:: test_rot_mat_orth rot_angles.(0);
  "orth test 2" >:: test_rot_mat_orth rot_angles.(1);
  "2d som test" >:: test_som_2d;
  "3d som test" >:: test_som_3d;
  "var order preservation" >:: test_som_3d_var_order dummy_vars;
]


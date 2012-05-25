open Test_util
open Som
open OUnit

(*open Minimization*)

let test_rot_mat_orth angles () =
  let orig = rot_mat angles in
  let dim = fst (Gsl_matrix.dims orig) in
  let trans = Gsl_matrix.create dim dim in
  Gsl_matrix.transpose trans orig;
  assert_equal orig trans

let rot_angles = [|
  [| 1.2; -0.3; 3.56 |];
  [| 0.; 2.1; -1.1 |]
|]

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

let minimized_trans = farrarr_of_string
"1.0  0.0  0.0  0.0  0.0
0.0  1.0  0.0  0.0  0.0
0.0  0.0  1.0  0.0  0.0
1.0  4.0  3.0  2.0 -8.0"

let mat_for_2d_som = farrarr_of_string
"0.9800666 0.1986693  0.0  0.0  0.0
-0.1986693 0.9800666  0.0  0.0  0.0
 0.0000000 0.0000000  1.0  0.0  0.0"

let mat_for_3d_som = farrarr_of_string
"0.8600893 0.1743487 -0.4794255  0.0  0.0
-0.4773022 0.6067681 -0.6356218  0.0  0.0
 0.1800803 0.7755224  0.6050918  0.0  0.0"

let dummy_vars = farr_of_string "0.5 0.25 0.15 0.1"

let test_som_2d () =
  let _, vects = som_rotation mat_for_2d_som 2 dummy_vars in
  "2d som incorrect" @? farrarr_approx_equal minimized_trans vects

let test_som_3d () =
  let _, vects = som_rotation mat_for_3d_som 3 dummy_vars in
  "3d som incorrect" @? farrarr_approx_equal minimized_trans vects

let suite = [
  "orth test 1" >:: test_rot_mat_orth rot_angles.(0);
  "orth test 2" >:: test_rot_mat_orth rot_angles.(1);
  "2d som test" >:: test_som_2d;
  "3d_som_test" >:: test_som_3d;
]

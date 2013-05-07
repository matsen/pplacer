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

let to_rotate = mat_of_string
"1.0  0.0  0.0  0.0  0.0
 0.0  1.0  0.0  0.0  0.0
 0.0  0.0  1.0  0.0  0.0"

let extra_row = farr_of_string "1.0  4.0  3.0  2.0 -8.0"

let we_would_like_this_back =
    Array.append (Gsl_matrix.to_arrays to_rotate) [| extra_row |]

let mat_for_som_test angles =
  let c = Gsl_matrix.copy to_rotate in
  Som.trans_a_mat_mul ~a:(rot_mat angles) ~b:to_rotate ~c;
  Array.append (Gsl_matrix.to_arrays c) [| extra_row |]

let dummy_vars = farr_of_string "0.5 0.25 0.15 0.1"

let test_som_2d angle () =
  let _, vects =
      som_rotation (mat_for_som_test [|angle; 0.; 0.|]) 2 dummy_vars in
  (* Ppr.print_float_array_array vects; *)
  "2d som incorrect" @? farrarr_approx_equal ~epsilon:1e-2 we_would_like_this_back vects

let test_som_3d angles () =
  let _, vects = som_rotation (mat_for_som_test angles) 3 dummy_vars in
  (* Ppr.print_float_array_array vects; *)
  "3d som incorrect" @?
      farrarr_approx_equal ~epsilon:5e-2 we_would_like_this_back vects

let test_som_3d_var_order orig_vars angles () =
  let vars, _ = som_rotation (mat_for_som_test angles) 3 orig_vars in
  let var_copy = Array.copy vars in
  Array.sort (fun x y -> compare y x) var_copy;
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
  "orth test 1" >:: test_rot_mat_orth [| 1.2; -0.3; 3.56 |];
  "orth test 2" >:: test_rot_mat_orth [| 0.; 2.1; -1.1 |];
  "2d som test 1" >:: test_som_2d 0.1;
  "2d som test 2" >:: test_som_2d (-0.4);
  "3d som test 1" >:: test_som_3d [|0.1; 0.2; 0.3|];
  "3d som test 2" >:: test_som_3d [|0.2; 0.5; -0.81|];
  "3d som test 3" >:: test_som_3d [|0.2;-0.9; 0.5|];
  "var order preservation" >:: test_som_3d_var_order dummy_vars [|0.1; 0.2; 0.3|];
]


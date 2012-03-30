
(* 3D rotation matrix *)
let rot_mat theta =
  Gsl_matrix.of_arrays [|
    [| cos theta ; (-1.) *. sin theta ; 0. |];
    [| sin theta ; cos theta        ; 0. |];
    [| 0.        ; 0.               ; 1. |] |];;

(* This does the actual rotations and returns the rotated matrix *)
let rotate_trans trans_part theta =
  let result = Gsl_matrix.copy trans_part in
  let mat_mult =
    Gsl_blas.gemm ~ta:Gsl_blas.Trans ~tb:Gsl_blas.NoTrans ~alpha: 1. ~beta: 0.0
  in
  mat_mult ~a:(rot_mat theta) ~b:trans_part ~c:result;
  result;;

(* Once we know the min theta, we can rotate the vars into place *)
let rotate_vars vars theta =
  let vars_part = Gsl_vector.of_array (Array.sub vars 0 3) in
  let vars_result = Gsl_vector.create ~init:0.0 3 in
  let vars_rest = Array.sub vars 3 ((Array.length vars) - 3) in
  let rot = rot_mat theta in
  (* Square the elements of the rotation matrix *)
  Gsl_matrix.mul_elements rot rot;
  (* Multiply it's transpose by our vars to get the rotated vars*)
  Gsl_blas.gemv Gsl_blas.Trans ~alpha:1.0 ~beta:0.0 ~a:rot ~x:vars_part ~y:vars_result;
  (* Merge and return *)
  Array.append (Gsl_vector.to_array vars_result) vars_rest;;

(* measures the overlap between the tranform vector components when rotated by
 * theta radians. Currently only set up for the 2D case... *)
let overlap trans_part theta =
  let rotated_trans = rotate_trans trans_part theta in
  let row i = Gsl_matrix.row rotated_trans i in
  let mult = row 0 in
  Gsl_vector.mul mult (row 1);
  Gsl_blas.asum mult;;

(* Performs overlap minimization using Brent *)
let min_overlap trans_part =
  let obj_fun = overlap trans_part in
  Minimization.brent obj_fun 0.0 (-. Gsl_math.pi_4) Gsl_math.pi_4 0.001;;

(*Returns a tuple of the roated trans (as an array of arrays), the rotated
 * vars, and the optimal theta value(s) *)
let som_rotation trans dimensions vars =
  let rotated_trans = Array.copy trans in
  let trans_part = Gsl_matrix.of_arrays (Array.sub trans 0 3) in
  let min_theta = min_overlap trans_part in
  Array.blit (Gsl_matrix.to_arrays (rotate_trans trans_part min_theta)) 0 rotated_trans 0 3;
  (rotate_vars vars min_theta, rotated_trans);;


(*let trans = [|*)
    (*[| 1.2; 0.35; (-2.1); 4.2 |];*)
    (*[| (-3.1); 0.12; 1.2; (-0.4) |];*)
    (*[| 2.1; 0.12; 1.2; (-3.2) |];*)
    (*[| 1.3; 5.4; (-2.1); (-3.2) |];*)
    (*[| 0.001; (-10.); 6.8; 3.4 |]*)
(*|];;*)
(*let vars = [| 0.5; 0.3; 0.2; 0.24; 0.21|];;*)

(*let results = som_rotation trans 2 vars;;*)


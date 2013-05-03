(*
Code to perform Support Overlap Minimzation.
- trans: the principal components
- trans_part: the first three rows of trans

*)

open Ppatteries

exception MinimizationError

(*
Rotation matrix in terms of Euler angles [|phi; theta; psi|]
http://mathworld.wolfram.com/EulerAngles.html

In[93]:= MatrixForm[Flatten[Table[{i,j},{i,0,2},{j,0,2}],1]]

Out[93]//MatrixForm= 0   0
                     0   1
                     0   2
                     1   0
                     1   1
                     1   2
                     2   0
                     2   1
                     2   2

In[94]:= MatrixForm[Flatten[toMatrix[ofEulerAngles[phi,theta,psi]],1]]

Out[94]//MatrixForm= Cos[phi] Cos[psi] - Cos[theta] Sin[phi] Sin[psi]
                     Cos[psi] Sin[phi] + Cos[phi] Cos[theta] Sin[psi]
                     Sin[psi] Sin[theta]
                     -(Cos[psi] Cos[theta] Sin[phi]) - Cos[phi] Sin[psi]
                     Cos[phi] Cos[psi] Cos[theta] - Sin[phi] Sin[psi]
                     Cos[psi] Sin[theta]
                     Sin[phi] Sin[theta]
                     -(Cos[phi] Sin[theta])
                     Cos[theta]
*)
let rot_mat angles  =
  let m = Gsl_matrix.create 3 3
  and cos_phi   = cos angles.(0)
  and sin_phi   = sin angles.(0)
  and cos_theta = cos angles.(1)
  and sin_theta = sin angles.(1)
  and cos_psi   = cos angles.(2)
  and sin_psi   = sin angles.(2)
  in
  m.{0,0} <- cos_phi*.cos_psi -. cos_theta*.sin_phi*.sin_psi;
  m.{0,1} <- cos_psi*.sin_phi +. cos_phi*.cos_theta*.sin_psi;
  m.{0,2} <-  sin_psi*.sin_theta;
  m.{1,0} <- -.(cos_psi*.cos_theta*.sin_phi) -. cos_phi*.sin_psi;
  m.{1,1} <-  cos_phi*.cos_psi*.cos_theta -. sin_phi*.sin_psi;
  m.{1,2} <-  cos_psi*.sin_theta;
  m.{2,0} <-  sin_phi*.sin_theta;
  m.{2,1} <-  -.cos_phi*.sin_theta;
  m.{2,2} <-  cos_theta;
  m

(*
When we are doing PCA, we want to transform our samples from the usual basis in
which they are expressed to the basis of k principal components. In the SOM
case, we want to apply a rotation to the plane spanned by the principal
component vectors. We can do this by multiplying the original kxp transormation
on the left by a kxk rotation.
*)
let rotate_trans trans_part angles =
  let result = Gsl_matrix.copy trans_part in
  let trans_mat_mult =
    Gsl_blas.gemm ~ta:Gsl_blas.Trans ~tb:Gsl_blas.NoTrans ~alpha:1. ~beta:0.
  in
  trans_mat_mult ~a:(rot_mat angles) ~b:trans_part ~c:result;
  result

(* Once we know the appropriate angles, we can rotate the vars into place. *)
let rotate_vars vars angles =
  let vars_part = Gsl_vector.of_array (Array.sub vars 0 3) in
  let vars_result = Gsl_vector.create ~init:0.0 3 in
  let vars_rest = Array.sub vars 3 ((Array.length vars) - 3) in
  let rot = rot_mat angles in
  (* Square the elements of the rotation matrix. *)
  Gsl_matrix.mul_elements rot rot;
  (* Multiply its transpose by our vars to get the rotated vars. *)
  Gsl_blas.gemv Gsl_blas.Trans ~alpha:1.0 ~beta:0.0 ~a:rot ~x:vars_part ~y:vars_result;
  Array.append (Gsl_vector.to_array vars_result) vars_rest

(* Measures the overlap between the tranform vector components when rotated
 * through the given angles. *)
let overlap trans_part dims angles =
  let rotated_trans = rotate_trans trans_part angles in
  let row i = Gsl_matrix.row rotated_trans i
  and indices = match dims with
  | 2 -> [(0, 1)]
  | 3 -> [(0, 1); (0, 2); (1, 2)]
  | _ -> failwith "Can only rotate in 2 or 3 dimensions\n"
  in
  let rec overlapper = function
  | [] -> 0.0
  | (i, j)::rest ->
      let mult = Gsl_vector.copy (row i) in
      Gsl_vector.mul mult (row j);
      (* asum because we want to take the absolute value dot product. *)
      Gsl_blas.asum mult +. overlapper rest
  in
  overlapper indices

(* Performs overlap minimization using Brent. *)
let min_overlap trans_part dims =
  let tolerance = (overlap trans_part dims [|0.; 0.; 0.|]) *. (0.0001) in
  match dims with
  | 2 ->
      let obj_fun phi = overlap trans_part dims [|phi; 0.; 0.|] in
      let min = Minimization.brent
        ~start_finder:Minimization.robust_start_finder
        obj_fun
        0.
        (-. Gsl_math.pi_4)
        Gsl_math.pi_4 tolerance
      in
      [|min; 0.; 0.|]
  | 3 ->
      let obj_fun = overlap trans_part dims
      and start = [|0.; 0.; 0.|]
      and lower = Array.make 3 (-. Gsl_math.pi)
      and upper = Array.make 3 (Gsl_math.pi)
      in begin
        try
          Minimization.multimin obj_fun start lower upper tolerance
        with
        | Minimization.ExceededMaxIter ->
            Printf.eprintf "MaxIterations exceeded in minimization routine";
            raise MinimizationError
        | Minimization.InvalidStartValues (left, start, right) ->
            Printf.eprintf "Invalid Brent Starting values: %g < %g < %g not \
            true\n"
                left start right;
            raise MinimizationError
        | Minimization.FindStartFailure ->
            Printf.eprintf "Was unable to find acceptable start values for \
            minimziation\n";
            raise MinimizationError
      end
  | _ -> failwith "Can only rotate in 2 or 3 dimensionss\n"

(* In situations where the roation takes the variances out of order, this
 * reorders both the vars and the trans vectors. *)
let reordered_by_vars vars trans dims =
  let reordered_vars = Array.copy vars
  and orig_var_i x = Array.findi (fun y -> y = x) vars
  in
  Array.sort (fun x y -> compare y x) reordered_vars;
  let reordered_trans = Array.map
    (fun var -> trans.(orig_var_i var))
    (Array.sub reordered_vars 0 dims)
  in
  (reordered_vars, reordered_trans)

(* A vector v has the same variance as -v; we would like whichever most
 * closely matches the original trans. This makes it easier to compare the
 * original to the SOM trans. *)
let flip_axes ~orig_trans ~new_trans =
  let flip vec = Array.map (fun x -> -1. *. x) vec in
  let flipper i vec =
    let flipped = flip vec in
    if Pca.dot vec orig_trans.(i) > Pca.dot flipped orig_trans.(i) then
      vec
    else
      flipped
  in
  Array.mapi flipper new_trans

(* Returns a tuple of the roated trans (as an array of arrays), the rotated
 * vars. *)
let som_rotation trans dims vars =
  let trans_part = Gsl_matrix.of_arrays (Array.sub trans 0 3) in
  (* Where all the real work is - find min(s) *)
  let min = min_overlap trans_part dims in
  let vars = rotate_vars vars min
  and trans_part = Gsl_matrix.to_arrays (rotate_trans trans_part min)
  in
  let vars, trans_part = reordered_by_vars vars trans_part dims in
  let flipped_trans = flip_axes ~orig_trans:trans ~new_trans:trans_part in
  (* We want our final trans to be the same as the original but with the first
   * dims dimensions rotated. *)
  let final_trans = Array.copy trans in
  Array.blit flipped_trans 0 final_trans 0 dims;
  (vars, final_trans)


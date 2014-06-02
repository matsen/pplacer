(*
Code to perform Support Overlap Minimzation as described in Matsen and Evans, 2013.
*)

open Ppatteries

exception MinimizationError


let trans_a_mat_mul =
  Gsl.Blas.gemm ~ta:Gsl.Blas.Trans ~tb:Gsl.Blas.NoTrans ~alpha:1. ~beta:0.

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
  let m = Gsl.Matrix.create 3 3
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

(* When we are doing PCA, we want to transform our samples from the usual basis
 * in which they are expressed to the basis of k principal components. In the
 * SOM case, we want to apply a rotation to the plane spanned by the principal
 * component vectors. If expressed in the usual way for transformations
 * (vectors are columns of transformation matrix) then we would multiply on the
 * right. However, in this case it's easier if the eigenvectors are the _rows_
 * of the eigenvector matrix, so we multiply on the left by the transpose.
*)
let rotate_vects vects_part angles =
  let result = Gsl.Matrix.copy vects_part in
  trans_a_mat_mul ~a:(rot_mat angles) ~b:vects_part ~c:result;
  result

(* We also need to "rotate" the variances, i.e. change them such that they
 * actually correspond to the rotated eigenvectors. Recall that
 * Var(aX + bY) = a^2 Var(X) + b^2 Var(Y) when X and Y are independent.
 * Thus, when we are looking at a linear transformation of a vector of
 * whose ith component is
 * \sum_j a_{ij} X_j
 * the variance is
 * \sum_j a_{ij}^2 Var(X_j).
 * As can be seen, we do this for a maximum of three dimensions. *)
let rotate_vals vals angles =
  let vals_part = Gsl.Vector.of_array (Array.sub vals 0 3) in
  let vals_result = Gsl.Vector.create ~init:0. 3 in
  let vals_rest = Array.sub vals 3 ((Array.length vals) - 3) in
  let rot = rot_mat angles in
  (* Square the elements of the rotation matrix. *)
  Gsl.Matrix.mul_elements rot rot;
  (* Multiply its transpose by our vals to get the rotated vals. *)
  Gsl.Blas.gemv Gsl.Blas.Trans ~alpha:1. ~beta:0. ~a:rot ~x:vals_part ~y:vals_result;
  Array.append (Gsl.Vector.to_array vals_result) vals_rest

(* Measures the overlap between the tranform vector components when rotated
 * through the given angles. *)
let overlap vects_part dims angles =
  let rotated_vects = rotate_vects vects_part angles in
  let row i = Gsl.Matrix.row rotated_vects i
  and indices = match dims with
  | 2 -> [(0, 1)]
  | 3 -> [(0, 1); (0, 2); (1, 2)]
  | _ -> failwith "Can only rotate in 2 or 3 dimensions\n"
  in
  let rec overlapper = function
  | [] -> 0.
  | (i, j)::rest ->
      let mult = Gsl.Vector.copy (row i) in
      Gsl.Vector.mul mult (row j);
      (* asum because we want to take the absolute value dot product. *)
      Gsl.Blas.asum mult +. overlapper rest
  in
  overlapper indices

(* Performs overlap minimization using Brent. *)
let min_overlap vects_part dims =
  let tolerance = (overlap vects_part dims [|0.; 0.; 0.|]) *. (0.0001) in
  match dims with
  | 2 ->
      let obj_fun phi = overlap vects_part dims [|phi; 0.; 0.|] in
      let min = Minimization.brent
        ~start_finder:Minimization.robust_start_finder
        obj_fun
        0.
        (-. Gsl.Math.pi_4)
        Gsl.Math.pi_4 tolerance
      in
      [|min; 0.; 0.|]
  | 3 ->
      let obj_fun = overlap vects_part dims
      and start = [|0.; 0.; 0.|]
      and lower = Array.make 3 (-. Gsl.Math.pi)
      and upper = Array.make 3 (Gsl.Math.pi)
      in
      let run_one_3d index_order =
          Minimization.multimin ~index_order obj_fun start lower upper tolerance
      in begin
      match
        List.reduce
          (fun pos1o pos2o -> match (pos1o,pos2o) with
            | (Some pos1, Some pos2) ->
                if obj_fun pos1 < obj_fun pos2 then Some pos1 else Some pos2
            | (Some pos, None) -> Some pos
            | (None, Some pos) -> Some pos
            | (None, None) -> None)
          (List.map
              (fun index_order ->
                try Some (run_one_3d index_order) with
                | Minimization.ExceededMaxIter
                | Minimization.InvalidStartValues _ (* (left, start, right) *)
                | Minimization.FindStartFailure -> None)
              [[|0;1;2|]; [|0;2;1|]; [|1;0;2|]; [|1;2;0|]; [|2;0;1|]; [|2;1;0|]])
        with
          | Some pos -> pos
          | None -> raise MinimizationError
          (* Sadly, there is little the user can do even if they know what the
           * source of the error is. *)
      end
  | _ -> failwith "Can only rotate in 2 or 3 dimensions\n"

(* In situations where the roation takes the variances out of order, this
 * reorders both the vals and the vects vectors. *)
let reordered_by_vals vals vects dims =
  let reordered_vals = Array.copy vals
  and orig_var_i x = Array.findi (fun y -> y = x) vals
  in
  Array.sort (fun x y -> compare y x) reordered_vals;
  let reordered_vects = Array.map
    (fun var -> vects.(orig_var_i var))
    (Array.sub reordered_vals 0 dims)
  in
  (reordered_vals, reordered_vects)

(* A vector v has the same variance as -v; we would like whichever most
 * closely matches the original vects. This makes it easier to compare the
 * original to the SOM vects. *)
let flip_axes ~orig_vects ~new_vects =
  let flip vec = Array.map (fun x -> -1. *. x) vec in
  let flipper i vec =
    let flipped = flip vec in
    if Pca.dot vec orig_vects.(i) > Pca.dot flipped orig_vects.(i) then
      vec
    else
      flipped
  in
  Array.mapi flipper new_vects

(* Returns a tuple of the roated vects (as an array of arrays), the rotated
 * vals. *)
let som_rotation vects dims vals =
  let vects_part = Gsl.Matrix.of_arrays (Array.sub vects 0 3) in
  (* Where all the real work is - find min(s) *)
  let min = min_overlap vects_part dims in
  let vals = rotate_vals vals min
  and vects_part = Gsl.Matrix.to_arrays (rotate_vects vects_part min)
  in
  let vals, vects_part = reordered_by_vals vals vects_part dims in
  let flipped_vects = flip_axes ~orig_vects:vects ~new_vects:vects_part in
  (* We want our final vects to be the same as the original but with the first
   * dims dimensions rotated. *)
  let final_vects = Array.copy vects in
  Array.blit flipped_vects 0 final_vects 0 dims;
  (vals, final_vects)



(* YIKES!!! *)
let rot_mat angles  =
  let c i = cos angles.(i)
  and s i = sin angles.(i) in
  Gsl_matrix.of_arrays [|
    [| (c 1)*.(c 2);  (-1.)*.(c 0)*.(s 2)+.(s 0)*.(s 1)*.(c 2);         (s 0)*.(s 2)+.(c 0)*.(s 1)*.(c 2)|];
    [| (c 1)*.(s 2);         (c 0)*.(c 2)+.(s 0)*.(s 1)*.(s 2);  (-1.)*.(s 2)*.(c 2)+.(c 0)*.(s 1)*.(s 2)|];
    [| (-1.)*.(s 1);         (s 0)*.(c 1)                     ;         (c 0)*.(c 1)                     |]
  |];;

(* This does the actual rotations and returns the rotated matrix *)
let rotate_trans trans_part angles =
  let result = Gsl_matrix.copy trans_part in
  let mat_mult =
    Gsl_blas.gemm ~ta:Gsl_blas.Trans ~tb:Gsl_blas.NoTrans ~alpha: 1. ~beta: 0.0
  in
  mat_mult ~a:(rot_mat angles) ~b:trans_part ~c:result;
  result;;

(* Once we know the min angles, we can rotate the vars into place *)
let rotate_vars vars angles =
  let vars_part = Gsl_vector.of_array (Array.sub vars 0 3) in
  let vars_result = Gsl_vector.create ~init:0.0 3 in
  let vars_rest = Array.sub vars 3 ((Array.length vars) - 3) in
  let rot = rot_mat angles in
  (* Square the elements of the rotation matrix *)
  Gsl_matrix.mul_elements rot rot;
  (* Multiply it's transpose by our vars to get the rotated vars*)
  Gsl_blas.gemv Gsl_blas.Trans ~alpha:1.0 ~beta:0.0 ~a:rot ~x:vars_part ~y:vars_result;
  (* Merge and return *)
  Array.append (Gsl_vector.to_array vars_result) vars_rest;;

(* measures the overlap between the tranform vector components when rotated
 * through the given angles. *)
let overlap trans_part dims angles =
  let rotated_trans = rotate_trans trans_part angles in
  let row i = Gsl_matrix.row rotated_trans i
  and indices = match dims with
  | 2 -> [(0, 1)]
  | 3 -> [(0, 1); (0, 2); (1, 2)]
  | _ -> failwith "Can only rotate in 2 or 3 dimensions"
  in
  let rec overlapper ls = match ls with
  | [] -> 0.0
  | (i, j)::ls' ->
      let mult = row i in
      Gsl_vector.mul mult (row j);
      (Gsl_blas.asum mult) +. overlapper(ls')
  in
  overlapper indices;;

(* Performs overlap minimization using Brent *)
let min_overlap trans_part dims = match dims with
  | 2 ->
      let obj_fun theta = overlap trans_part dims [|0.0; 0.0; theta|] in
      let min = Minimization.brent obj_fun 0.0 (-. Gsl_math.pi_4) Gsl_math.pi_4 0.001 in
      [|0.; 0.; min|]
  | 3 ->
      let obj_fun = overlap trans_part dims
      and start = [|0.; 0.; 0.|]
      and lower = Array.make 3 (Gsl_math.pi /. (-4.))
      and upper = Array.make 3 (Gsl_math.pi /. (4.))
      in
      Minimization.multimin obj_fun start lower upper 0.001
  | _ -> failwith "Can only rotate in 2 or 3 dimensions"

(* Returns a tuple of the roated trans (as an array of arrays), the rotated
 * vars, and the optimal theta value(s) *)
let som_rotation trans dims vars =
  let rotated_trans = Array.copy trans in
  let trans_part = Gsl_matrix.of_arrays (Array.sub trans 0 3) in
  let min = min_overlap trans_part dims in
  Array.blit (Gsl_matrix.to_arrays (rotate_trans trans_part min)) 0 rotated_trans 0 3;
  (rotate_vars vars min, rotated_trans);;


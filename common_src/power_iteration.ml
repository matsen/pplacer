(* Implements power iteration to get the absolute value of the largest
 * eigenvalue (in absolute value) of a symmetric matrix.
 *
 * http://en.wikipedia.org/wiki/Power_iteration
*)

open Linear_utils

type eigen =
  {
    v: Gsl_vector.vector;
    l: float;
  }

let scale_by_l2 v = Gsl_vector.scale v (1. /. (Gsl_blas.nrm2 v))

(* this function returns true if the l-infinity difference between (v * w[0]/v[0]) and w
 * is less than some tolerance.
 * *)
let stop_time tol v w =
  let pseudo_eval = w.{0} /. v.{0} in
  try
    vec_iter2
      (fun vi wi ->
        if abs_float (wi -. (pseudo_eval *. vi)) > tol then
          raise Exit)
      v
      w;
    true
  with
  | Exit -> false

(* Find the top eigenvalue of a symmetric matrix by power iteration. *)
let top_eig m tol max_iter =
  let (rows, cols) = Gsl_matrix.dims m in
  assert(rows = cols);
  let v = Gsl_vector.create ~init:1. rows in
  let scratch = Gsl_vector.copy v in
  let mat_vec_mul ~a =
    Gsl_blas.symv Gsl_blas.Upper ~alpha:1. ~a ~beta:0. in
  let mul_and_scale x dst =
    mat_vec_mul ~a:m ~x ~y:dst;
    scale_by_l2 dst
  in
  let rec aux iter_n =
    if iter_n < max_iter then begin
      (* we do a back-and-forth to preserve the roles of scratch and v *)
      mul_and_scale v scratch;
      mul_and_scale scratch v;
      if not (stop_time tol scratch v) then aux (iter_n+1)
    end
    else failwith "top_eig: exceeded maximum number of iterations"
  in
  aux 0;
  mat_vec_mul ~a:m ~x:v ~y:scratch;
  {
    l = scratch.{0} /. v.{0};
    v = v;
  }

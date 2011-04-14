(* Implements power iteration to get the absolute value of the largest
 * eigenvalue (in absolute value) of a symmetric matrix.
 *
 * http://en.wikipedia.org/wiki/Power_iteration
*)

open Linear_utils
open Bigarray

type eig =
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

(* Calculates the outer product (scalar v v^T) and puts it in m. *)
let outer_product ?(scalar=1.) m v =
  let len = Gsl_vector.length v in
  let (n_rows, n_cols) = Gsl_matrix.dims m in
  assert(n_rows = len && n_cols = len);
  for i=0 to len-1 do
    let row = Array2.slice_left m i in
    Array1.blit v row; (* copy v to row *)
    Gsl_vector.scale row (scalar *. v.{i});
  done;;

let m = Gsl_matrix.create 3 3;;

let v = Gsl_vector.of_array [|1.; 5.; 2.|];;

outer_product ~scalar:(-1.) m v;;
m;;

let projector_of_eig m eig =
  outer_product ~scalar:eig.l m eig.v

(* make a list of the top n eigs *)
let top_eigs m tol max_iter n_eigs =
  let m' = Gsl_matrix.copy m in
  let proj = Gsl_matrix.copy m in
  let rec aux n_left accu =
    if n_left <= 0 then accu
    else
      let eig = top_eig m' tol max_iter in
      projector_of_eig proj eig;
      (* Subtract the projector from m'. *)
      Gsl_matrix.sub m' proj;
      aux (n_left - 1) (eig::accu)
  in
  List.rev (aux n_eigs [])



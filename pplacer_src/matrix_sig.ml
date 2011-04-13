(* To get significance values using eigenvalues of the common ancestry matrix.
 *
 * This code is not practically useful (matrices get too big), so is not exposed
 * to the outside except for unit testing.
 *
 * mtilde = \int_u G_i(u) G_j(u) \lambda(du)
 *
 * see scans: matrix_sig.pdf
 *)


open Bigarray
open Fam_batteries
open Linear_utils

let rooted_qform m v = sqrt(qform m v)

(* *** Unit test code ***
 *
 * This code uses a different (matrix based) way of calculating KR, and so is a
 * nice way to verify that our calculations are correct. *)

let matrix_of_pqueries weighting criterion t pqueryl =
  let pquerya = Array.of_list pqueryl in
  let n = Array.length pquerya
  and ca_info = Edge_rdist.build_ca_info t
  in
  let m = Gsl_matrix.create n n in
  let dist_fun =
    (Pquery_distances.dist_fun_of_weight weighting) criterion ca_info
  in
  for i=0 to n-1 do
    for j=i to n-1 do
      let x = dist_fun pquerya.(i) pquerya.(j) in
      Array2.unsafe_set m i j x;
      if i <> j then Array2.unsafe_set m j i x;
    done
  done;
  m

(* could be made faster by improving the way the matrices are accessed *)
let build_mtilde weighting criterion pr1 pr2 =
  let t = Placerun.get_same_tree pr1 pr2 in
  let mt = matrix_of_pqueries weighting criterion t
      ((Placerun.get_pqueries pr1)@(Placerun.get_pqueries pr2))
  in
  Gsl_matrix.scale mt (1. /. (Gtree.tree_length t));
  mt

let vec_tot = vec_fold_left (+.) 0.

(* The distance calculated by using matrices rather than by using integration
 * along edges. *)
let matrix_distance weighting criterion pr1 pr2 =
  let n1 = Placerun.n_pqueries pr1
  and n2 = Placerun.n_pqueries pr2 in
  let inv_n1 = 1. /. (float_of_int n1)
  and neg_inv_n2 = -. 1. /. (float_of_int n2) in
  let indicator =
    vec_init
      (n1+n2)
      (fun i -> if i < n1 then inv_n1 else neg_inv_n2)
  in
  let mtilde = build_mtilde weighting criterion pr1 pr2 in
  rooted_qform mtilde indicator


(* *** Research Code ***
 *
 * This code isn't used for anything in guppy, but implements some interesting
 * directions in the KR paper.
 * *)

let max_iter = 100
let tol = 1e-5
let sq v = v *. v

(* given an f which takes a vector and gives a float, make a vector out of
 * applying f to each of the rows of m *)
let map_rows_to_vector f m =
  let n_rows = Array2.dim1 m in
  let v = Gsl_vector.create n_rows in
  for i=0 to n_rows-1 do
    Array1.unsafe_set v i (f (Gsl_matrix.row m i))
  done;
  v

(* the average of each of the rows *)
let row_avg m =
  let (_, n_cols) = Gsl_matrix.dims m
  and ra = map_rows_to_vector vec_tot m in
  Gsl_vector.scale ra (1. /. (float_of_int n_cols));
  ra

(* if m is mtilde then convert it to an m. n1 and n2 are the number of pqueries
 * in pr1 and pr2 *)
let m_of_mtilde m n1 n2 =
  (* it's mtilde so far *)
  let (n,_) = Gsl_matrix.dims m in
  assert(n = n1+n2);
  let ra = row_avg m in
  let avg = (vec_tot ra) /. (float_of_int n) in
  let coeff = 1. /. (float_of_int (n1*n2)) in
  for i=0 to n-1 do
    for j=0 to n-1 do
      Array2.unsafe_set m i j
        (coeff *.
          ((Array2.unsafe_get m i j)
          -. (Array1.unsafe_get ra i)
          -. (Array1.unsafe_get ra j)
          +. avg))
    done
  done


(* get expectation of w within some tolerance *)
let w_expectation rng tol m =
  let n = mat_dim_asserting_square m in
  let v = Gsl_vector.create n in
  let n_samples = ref 0
  and sample_total = ref 0.
  in
  let next_expectation () =
    for i=0 to n-1 do
      v.{i} <- Gsl_randist.gaussian rng ~sigma:1.
    done;
    incr n_samples;
    sample_total := !sample_total +. rooted_qform m v;
    (!sample_total) /. (float_of_int (!n_samples))
  in
  (* make sure we get at least 10 (one more below) *)
  for i=1 to 10 do let _ = next_expectation () in () done;
  (* continue until the expectation changes less than tol *)
  let rec get_expectation prev =
    let ew = next_expectation () in
    if abs_float (ew -. prev) > tol then get_expectation ew
    else ew
  in
  get_expectation (next_expectation ())

let write_matrix_normal_dist rng name1 name2 m w n_samples =
  let n = Array2.dim1 m in
  let v = Gsl_vector.create n in
  let normal_ws =
    ListFuns.init
      n_samples
      (fun _ ->
        for i=0 to n-1 do
          v.{i} <- Gsl_randist.gaussian rng ~sigma:1.
        done;
        rooted_qform m v)
  in
  R_plots.write_density 2. "matrix_normal" name1 name2 w normal_ws

let matrix_distribution weighting criterion rng pr1 pr2 =
  let n1 = Placerun.n_pqueries pr1
  and n2 = Placerun.n_pqueries pr2 in
    (* first m is mtilde *)
  let m = build_mtilde weighting criterion pr1 pr2 in
  (* then it actually becomes mtilde *)
  m_of_mtilde m n1 n2;
  let w = matrix_distance weighting criterion pr1 pr2 in
  let ew = w_expectation rng tol m in
  Printf.printf "W: %g\t E[W]: %g\n" w ew;
  write_matrix_normal_dist rng
    (Placerun.get_name pr1) (Placerun.get_name pr2)
    m w 1000

(* trace-msq is the trace of m^2 *)
let trace_msq m =
  let n = Array2.dim1 m in
  assert(n = (Array2.dim2 m));
  let x = ref 0. in
  for i=0 to n-1 do
    x := (!x) +. sq (Array2.unsafe_get m i i)
  done;
  !x


(* This section implements power iteration to get the absolute value of the
 * largest eigenvalue (in absolute value) of a symmetric matrix.
 *
 * http://en.wikipedia.org/wiki/Power_iteration
*)

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

(* find the top eigenvalue of a symmetric matrix by power iteration.
 *)
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
      mul_and_scale v scratch;
      mul_and_scale scratch v;
      if not (stop_time tol scratch v) then aux (iter_n+1)
    end
    else failwith "top_eig: exceeded maximum number of iterations"
  in
  aux 0;
  mat_vec_mul ~a:m ~x:v ~y:scratch;
  scratch.{0} /. v.{0}


(* This code is for Borell's inequality. Although it does give a bound, it's
 * much too conservative to be useful. *)
let borell_sig m w ew =
  Printf.printf "tr M: %g\n" (trace m);
  Printf.printf "2 tr M^2: %g\n" (2. *. (trace_msq m));
  Printf.printf "sqrt(2 tr M^2): %g\n" (sqrt (2. *. (trace_msq m)));
  let t = w -. ew in
  (w,
  2. *. exp ( -. t *. t /. (2. *. (top_eig m tol max_iter))))
  (* this is 2 \exp ( \frac{(-t^2}{2 max_eig m} ) *)


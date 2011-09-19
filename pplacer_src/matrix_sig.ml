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
open Ppatteries
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
    List.init
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


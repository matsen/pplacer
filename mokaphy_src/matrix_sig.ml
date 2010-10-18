(* mokaphy v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * to get significance values using eigenvalues of the common ancestry matrix.
 *
 * mtilde = \int_u G_i(u) G_j(u) \lambda(du)
 *
 * see scans: matrix_sig.pdf
 *)


open Fam_batteries

let sq v = v *. v
let tol = 1e-5
let max_iter = 100

module BA1 = Bigarray.Array1
module BA2 = Bigarray.Array2

(* could be made faster by improving the way the matrices are accessed *)
let build_mtilde weighting criterion pr1 pr2 = 
  let t = Placerun.get_same_tree pr1 pr2 in
  let mt = 
    Pquery_distances.of_pqueries weighting criterion t
      ((Placerun.get_pqueries pr1)@(Placerun.get_pqueries pr2))
  in
  Gsl_matrix.scale mt (1. /. (Gtree.tree_length t));
  mt

let vec_tot = Fam_vector.fold_left (+.) 0.

(* the average of each of the rows *)
let row_avg m = 
  let (_, n_cols) = Gsl_matrix.dims m
  and ra = Fam_matrix.map_rows_to_vector vec_tot m in
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
      BA2.unsafe_set m i j
        (coeff *.
          ((BA2.unsafe_get m i j) 
          -. (BA1.unsafe_get ra i)
          -. (BA1.unsafe_get ra j)
          +. avg))
    done
  done

let rooted_qform m v = sqrt(Fam_matrix.qform m v)

(* get expectation of w within some tolerance *)
let w_expectation rng tol m = 
  let n = Fam_matrix.dim_asserting_square m in
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
  let n = BA2.dim1 m in
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
  R_plots.write_density "matrix_normal" name1 name2 w normal_ws 2.

let write_eigs m =
  let ch = open_out "eigs.out" in
  Fam_vector.iter
    (fun x -> Printf.fprintf ch "%g\n" x)
    (Gsl_eigen.symm (`M(m)));
  close_out ch

(* trace-msq is the trace of m^2 *)
let trace_msq m = 
  let n = BA2.dim1 m in
  assert(n = (BA2.dim2 m));
  let x = ref 0. in
  for i=0 to n-1 do
    x := (!x) +. sq (BA2.unsafe_get m i i)
  done;
  !x

let dist_and_p weighting criterion rng pr1 pr2 = 
  let n1 = Placerun.n_pqueries pr1 
  and n2 = Placerun.n_pqueries pr2 in
  let inv_n1 = 1. /. (float_of_int n1)
  and neg_inv_n2 = -. 1. /. (float_of_int n2) in
  let indicator = 
    Fam_vector.init 
      (n1+n2)
      (fun i -> if i < n1 then inv_n1 else neg_inv_n2)
  in
  let m = build_mtilde weighting criterion pr1 pr2 in
    (* first m is mtilde *)
  let w = rooted_qform m indicator in
  (* then it actually becomes mtilde *)
  m_of_mtilde m n1 n2;
  let ew = w_expectation rng tol m in
  Printf.printf "W: %g\t E[W]: %g\n" w ew;
  write_matrix_normal_dist rng 
    (Placerun.get_name pr1) (Placerun.get_name pr2)
    m w 1000;
  Printf.printf "tr M: %g\n" (Fam_matrix.trace m);
  Printf.printf "2 tr M^2: %g\n" (2. *. (trace_msq m));
  Printf.printf "sqrt(2 tr M^2): %g\n" (sqrt (2. *. (trace_msq m)));
  let t = w -. ew in
  (w,
  2. *. exp ( -. t *. t /. (2. *. (Top_eig.top_eig m tol max_iter))))
  (* this is 2 \exp ( \frac{(-t^2}{2 max_eig m} ) *)



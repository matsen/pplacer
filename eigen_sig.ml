(* mokaphy v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * to get significance values using eigenvalues of the common ancestry matrix.
 *
 * mtilde = \int_u G_i(u) G_j(u) \lambda(du)
 *)


module BA1 = Bigarray.Array1
module BA2 = Bigarray.Array2

let build_mtilde use_pp pr1 pr2 = 
  let t = Placerun.get_same_tree pr1 pr2 in
  let both = 
    Array.of_list
    ((Placerun.get_pqueries pr1)@(Placerun.get_pqueries pr2)) in
  let n = Array.length both
  and get_weight = 
    if use_pp then Placement.post_prob else Placement.ml_ratio
  and ca_info = Camat.build_ca_info t
  in
  let mt = Gsl_matrix.create ~init:0. n n in
  (* increment mt[i][j] by x symmetrically *)
  let mt_increment i j x =
    let incr_one i j = 
      BA2.unsafe_set mt i j 
        ((BA2.unsafe_get mt i j) +. x) in
    incr_one i j;
    if i <> j then incr_one j i;
  in
  for i=0 to n-1 do
    for j=i to n-1 do
      Base.list_iter_over_pairs_of_two 
        (fun p1 p2 ->
          mt_increment i j 
            ((get_weight p1) *. (get_weight p2) *.
              ((Camat.find_ca_dist ca_info
                (Placement.location p1, Placement.distal_bl p1)
                (Placement.location p2, Placement.distal_bl p2)))))
        (Pquery.place_list both.(i))
        (Pquery.place_list both.(j))
    done
  done;
  Gsl_matrix.scale mt (1. /. (Gtree.tree_length t));
  mt

let vec_tot = Fam_vector.fold_left (+.) 0.

(* the average of each of the rows *)
let row_avg m = 
  let (_, n_cols) = Gsl_matrix.dims m
  and ra = Fam_matrix.map_rows_to_vector vec_tot m in
  Gsl_vector.scale ra (1. /. (float_of_int n_cols));
  ra

let build_m use_pp pr1 pr2 = 
  let m = build_mtilde use_pp pr1 pr2 in
  (* it's mtilde so far *)
  let (n,_) = Gsl_matrix.dims m in
  let ra = row_avg m in
  let avg = (vec_tot ra) /. (float_of_int n) in
  let coeff = 
    (float_of_int n) /.
    (float_of_int 
      ((Placerun.n_pqueries pr1) * (Placerun.n_pqueries pr2)))
  in
  for i=0 to n-1 do
    for j=0 to n-1 do
      BA2.unsafe_set m i j
        (coeff *.
          ((BA2.unsafe_get m i j) 
          -. (BA1.unsafe_get ra i)
          -. (BA1.unsafe_get ra j)
          +. avg))
    done
  done;
  m

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
    sample_total := !sample_total +. (rooted_qform m v);
    (!sample_total) /. (float_of_int (!n_samples))
  in
  (* make sure we get at least 10 (one more below) *)
  for i=1 to 10 do let _ = next_expectation () in () done;
  (* STEVE: continue until the expectation changes less than tol *)
  let rec get_expectation prev = 
    let ew = next_expectation () in
    if abs_float (ew -. prev) > tol then get_expectation ew
    else ew
  in
  get_expectation (next_expectation ())

let matrix_pvalue use_pp pr1 pr2 = 
  let n1 = Placerun.n_pqueries pr1 
  and n2 = Placerun.n_pqueries pr2 in
  let indicator = 
    Fam_vector.init 
      (n1+n2)
      (fun i -> if i < n1 then 1. else 0.)
  in
  let m = build_m use_pp pr1 pr2 in
  (* (rooted_qform m indicator, w_expectation rng 1e-5 m) *)
  rooted_qform m indicator


  (* Gsl_blas.dot x (Fam_gsl_matvec.allocMatVecMul a x) *)

  let x = Placerun_io.of_file

(* mokaphy v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * implements power iteration to get the absolute value of the largest
 * eigenvalue (in absolute value) of a symmetric matrix.
 *
 * http://en.wikipedia.org/wiki/Power_iteration
*)

let max_iter = 100

let scale_by_l2 v = Gsl_vector.scale v (1. /. (Gsl_blas.nrm2 v))

let v_diff = Fam_vector.map2_into (-.)

(* this function returns true if the l-infinity difference between (v * w[0]/v[0]) and w
 * is less than some tolerance. 
 * *)
let stop_time tol v w = 
  let pseudo_eval = w.{0} /. v.{0} in
  try
    Fam_vector.iter2
      (fun vi wi ->
        if abs_float (wi -. (pseudo_eval *. vi)) > tol then
          raise Exit)
      v 
      w;
    true
  with
  | Exit -> false

(* find the top eigenvalue of a symmetric matrix by power iteration.
 * mul_fun is a BLAS function for multiplication of a vector times a matrix.
 *)
let top_eig m tol max_iter = 
  let (rows, cols) = Gsl_matrix.dims m in
  assert(rows = cols);
  let v = Gsl_vector.create ~init:1. rows in
  let scratch = Fam_vector.mimic v in
  let mat_vec_mul ~a = 
    Gsl_blas.symv Gsl_blas.Upper ~alpha:1. ~a ~beta:0. in
  let mul_and_scale x dst = 
    mat_vec_mul ~a:m ~x ~y:dst;
    scale_by_l2 dst
  in
  let rec aux iter_n = 
    Printf.printf "iter %d\n" iter_n;
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


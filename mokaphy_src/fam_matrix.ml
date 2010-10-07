(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

module BA1 = Bigarray.Array1
module BA2 = Bigarray.Array2

let dim_asserting_square m = 
  let n = BA2.dim1 m in
  assert(n = BA2.dim2 m);
  n

let init n_rows n_cols f =
  let m = Gsl_matrix.create n_rows n_cols in
  for i=0 to n_rows-1 do
    let row = BA2.slice_left m i in
    for j=0 to n_cols-1 do
      BA1.unsafe_set row j (f i j)
    done;
  done;
  m

(* given an f which takes a vector and gives a float, make a vector out of
 * applying f to each of the rows of m *)
let map_rows_to_vector f m = 
  let n_rows = BA2.dim1 m in
  let v = Gsl_vector.create n_rows in
  for i=0 to n_rows-1 do
    BA1.unsafe_set v i (f (Gsl_matrix.row m i))
  done;
  v

let qform m v = 
  let n = Gsl_vector.length v in
  assert(n = BA2.dim1 m && n = BA2.dim2 m);
  let x = ref 0. in
  for i=0 to n-1 do
    let vi = BA1.unsafe_get v i 
    and mi = BA2.slice_left m i 
    in
    for j=0 to n-1 do
      x := 
        (!x) +. vi *. (BA1.unsafe_get v j) *. (BA1.unsafe_get mi j)
    done;
  done;
  !x

let trace m = 
  let n = BA2.dim1 m in
  assert(n = (BA2.dim2 m));
  let x = ref 0. in
  for i=0 to n-1 do
      x := (!x) +. (BA2.unsafe_get m i i)
  done;
  !x

let ppr ff m = 
  let nrows = BA2.dim1 m in
  Format.fprintf ff "@[{";
  for i=0 to nrows-1 do
    Fam_vector.ppr ff (Gsl_matrix.row m i);
    if i < nrows-1 then Format.fprintf ff ";@ "
  done;
  Format.fprintf ff "}@]"

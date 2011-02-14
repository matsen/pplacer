(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

module BA = Bigarray
module BA3 = BA.Array3

type tensor = (float, BA.float64_elt, BA.c_layout) BA.Array3.t

let dim1 = BA3.dim1
let dim2 = BA3.dim2
let dim3 = BA3.dim3
let create a b c = BA3.create BA.float64 BA.c_layout a b c
let mimic a = create (BA3.dim1 a) (BA3.dim2 a) (BA3.dim3 a)
let copy a = let b = mimic a in BA3.blit a b; b
let set_all (* tensor value *) = BA3.fill

let ppr ff x =
  let n = dim1 x in
  Format.fprintf ff "@[{";
  for i=0 to n-1 do
    Fam_gsl_matvec.ppr_gsl_matrix ff (BA3.slice_left_2 x i);
    if i < n-1 then Format.fprintf ff ";@ "
  done;
  Format.fprintf ff "}@]"


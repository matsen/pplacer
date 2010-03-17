(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * just the calls for linear_c.
*)

external glv_print : Tensor.tensor -> unit = "glv_print_c"

(* statd x y z util *)
external log_like3 : Gsl_vector.vector -> Tensor.tensor -> Tensor.tensor -> Tensor.tensor -> Gsl_vector.vector -> float = "log_like3_c"

(* dst x y *)
external pairwise_prod : Tensor.tensor -> Tensor.tensor -> Tensor.tensor -> unit = "pairwise_prod_c"

(* dst = a * b *)
external gemmish : Gsl_matrix.matrix -> Gsl_matrix.matrix -> Gsl_matrix.matrix -> unit = "gemmish_c"

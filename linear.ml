(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
*)

(*
external print_00 : Gsl_matrix.matrix -> unit = "print_00_c"
*)

external dot : Gsl_vector.vector -> Gsl_vector.vector -> int -> float = "dot_c"
external triple_dot : Gsl_vector.vector -> Gsl_vector.vector -> Gsl_vector.vector -> int -> float = "triple_dot_c"
external quad_dot : Gsl_vector.vector -> Gsl_vector.vector -> Gsl_vector.vector -> Gsl_vector.vector -> int -> float = "quad_dot_c"
external pairwise_prod : Gsl_vector.vector -> Gsl_vector.vector -> Gsl_vector.vector -> unit = "pairwise_prod_c"


(*
let dot x y size = 
  let tot = ref 0. in
  for i=0 to size - 1 do
    tot := !tot +. x.{i} *. y.{i}
  done;
  !tot

let triple_dot x y z size = 
  let tot = ref 0. in
  for i=0 to size - 1 do
    tot := !tot +. x.{i} *. y.{i} *. z.{i}
  done;
  !tot

let quad_dot x y z w size = 
  let tot = ref 0. in
  for i=0 to size - 1 do
    tot := !tot +. x.{i} *. y.{i} *. z.{i} *. w.{i}
  done;
  !tot

let pairwise_prod dest x y size =
  for i=0 to size - 1 do
    dest.{i} <- x.{i} *. y.{i}
  done

let triplewise_prod dest x y z size =
  for i=0 to size - 1 do
    dest.{i} <- x.{i} *. y.{i} *. z.{i}
  done
*)

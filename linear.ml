(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
*)


external dot : Gsl_vector.vector -> Gsl_vector.vector -> int -> float = "dot_c"
external triple_dot : Gsl_vector.vector -> Gsl_vector.vector -> Gsl_vector.vector -> int -> float = "triple_dot_c"
external quad_dot : Gsl_vector.vector -> Gsl_vector.vector -> Gsl_vector.vector -> Gsl_vector.vector -> int -> float = "quad_dot_c"
external pairwise_prod : Gsl_vector.vector -> Gsl_vector.vector -> Gsl_vector.vector -> unit = "pairwise_prod_c"


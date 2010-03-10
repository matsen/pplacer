(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

module BA1 = Bigarray.Array1

let mimic v = Gsl_vector.create (Gsl_vector.length v)

let init n f = 
  let v = Gsl_vector.create n in
  for i=0 to n-1 do 
    BA1.unsafe_set v i (f i)
  done;
  v

let fold_left f start v = 
  let x = ref start
  and n = Gsl_vector.length v in
  for i=0 to n-1 do 
    x := f !x (BA1.unsafe_get v i)
  done;
  !x


let iter2 f a b = 
  let n = Gsl_vector.length a in
  assert(n = Gsl_vector.length a);
  for i=0 to n-1 do
    f (BA1.unsafe_get a i) (BA1.unsafe_get b i)
  done

let map2_into f ~dst a b = 
  let n = Gsl_vector.length dst in
  assert(n = Gsl_vector.length a);
  assert(n = Gsl_vector.length b);
  for i=0 to n-1 do
    BA1.unsafe_set dst i
      (f (BA1.unsafe_get a i) (BA1.unsafe_get b i))
  done

let ppr ff y = 
  Format.fprintf ff "@[{";
  Ppr.ppr_list_inners Format.pp_print_float ff (
    Array.to_list (Gsl_vector.to_array y));
  Format.fprintf ff "}@]"



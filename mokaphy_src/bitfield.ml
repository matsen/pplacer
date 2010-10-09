(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Here we define a bitfield type packed into integers. 
 *
 * I've set it up to work like an array, with getting and setting. 
 * ** However ** these are not mutable, which is the point-- we can then use
 * them as keys for maps and indices for arrays.
 * The pretty printing and the array access are done in order of indices, rather
 * than using the binary setup.
 * However, information is stored as ei = 2^i... so if you are looking at the
 * binary number then it is the reverse of the pretty printing and access.
 *)

type t = int

let max_bits = Nativeint.size - 1

let empty = 0

(* typing is done in the mli file *)
let of_int i = i
let to_int b = b

(* basics *)
let check_bounds i = 
  if i < 0 || i >= max_bits then invalid_arg "index out of bounds"

let get (b:t) i = 
  check_bounds i;
  0 <> ((b lsr i) mod 2)

let ei i = 1 lsl i

let set (b:t) i = 
  check_bounds i;
  b lor (ei i)

let clear (b:t) i = 
  check_bounds i;
  b land (lnot (ei i))

let compare (b:t) (b':t) = b - b'

(* functions *)
let get_pos_indices (b:t) = 
  let rec aux acc (i:int) = 
    if i < 0 then acc
    else aux (if get b i then i::acc else acc) (i-1)
  in
  aux [] (max_bits-1)

(* io *)
let to_string (b:t) = 
  let s = String.create max_bits in
  for i=0 to max_bits-1 do
    s.[i] <- if get b i then '0' else '1';
  done;
  s

let ppr ff (b:t) = Format.pp_print_string ff (to_string b)


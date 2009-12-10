(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open Gc

let word_size = Sys.word_size
let bytes_conversion_factor = word_size / 8
let bytes_of_words words = words * bytes_conversion_factor

let curr_words () = 
  let s = quick_stat ()
  and c = get () 
  in
  s.heap_words + c.minor_heap_size

let curr_bytes () = bytes_of_words (curr_words ())

let ceiling_collection ceiling = 
  if ceiling < curr_bytes () then begin Gc.compact (); true end
  else false

let check_ceiling ceiling = 
  let cb = curr_bytes () in
  if ceiling < cb then 
    Printf.printf 
      "current memory use of %d exceeds ceiling of %d\n" cb ceiling

let bytes_of_kb b = int_of_float (1e3 *. b)
let bytes_of_mb b = int_of_float (1e6 *. b)
let bytes_of_gb b = int_of_float (1e9 *. b)

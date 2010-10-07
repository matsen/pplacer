(* Copyright (C) 2009  Frederick A Matsen.
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


let time_str_of_seconds secs = 
  let iSecs = int_of_float secs in
  let hours = iSecs / 3600 in
  let mins = iSecs / 60 - 60*hours in
  Printf.sprintf "%02d:%02d:%02.1f" hours mins
    (secs -. (float_of_int (hours*3600)) -. (float_of_int (mins*60)))

let print_elapsed_time () =
  let t = Sys.time () in
  Printf.printf "elapsed time: %s (%g s)\n" (time_str_of_seconds t) t

let calc_total_memory () = 
  let stat = Gc.quick_stat () in
  (stat.Gc.minor_words +. 
   stat.Gc.major_words -. 
   stat.Gc.promoted_words)
  *. (float_of_int Sys.word_size) /. 8.

let print_memory_usage () = 
  Printf.printf "memory usage (bytes): %g\n" (calc_total_memory ())

let print_n_compactions () = 
  Printf.printf "number of garbage compactions: %d\n" 
                ((Gc.quick_stat ()).Gc.compactions)

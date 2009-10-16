(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets
open Fam_batteries

let ppr_kr_info ff info_map = 
  IntMapFuns.ppr_gen
    (Ppr.ppr_list
      (fun ff (bl, v) -> 
        Format.fprintf ff 
          "(%g, %a)" 
          bl
          Ppr.ppr_float_array v))
    ff
    info_map

let ppr_pair_float_uptri ff fu = 
  Uptri.ppr_uptri 
    (fun ff (x,y) -> Format.fprintf ff "(%g, %g)" x y)
    ff 
    fu

let write_named_float_uptri ch names u =
  String_matrix.write_named_padded ch names
    (MatrixFuns.map 
      (Printf.sprintf "%g")
      (Uptri.to_matrix (fun _ -> 0.) u))


(* add v2 to v1 (which is modified in place) *)
let v_addto v1 v2 = 
  for i=0 to (Array.length v1)-1 do
    v1.(i) <- v1.(i) +. v2.(i)
  done

(* multiply a vector times a scalar. functional. *)
let v_times_scalar v s =
  Array.map (( *. ) s) v

(* take the vector sum of a float array list. no side effects. *)
let v_list_sum = function
  | hd::tl ->
    let v = Array.copy hd in
    List.iter (v_addto v) tl;
    v
  | [] -> assert(false)


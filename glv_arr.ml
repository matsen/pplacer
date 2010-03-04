(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * just routines for arrays of Glv's.
*)

type glv_arr = Glv.glv array

let arr_get = Array.get

let make ~n_glvs ~n_sites ~n_rates ~n_states = 
  Array.init n_glvs (fun _ -> Glv.make ~n_sites ~n_rates ~n_states)

let iter = Array.iter

let copy a = Array.map Glv.copy a

let mimic a = Array.map Glv.mimic a

let get a glvi = arr_get a glvi

let get_one a = assert(a <> [||]); a.(0)

(* let mask mask_arr a = Array.map (Glv.mask mask_arr) a *)

let evolve_into model ~src ~dst bl_fun = 
  let n = Array.length src in
  if n <> Array.length dst then
    failwith "Glv_arr.evolve_into: unequal lengths!";
  for i=0 to n-1 do
    Glv.evolve_into model ~src:src.(i) ~dst:dst.(i) (bl_fun i)
  done

let perhaps_pull_exponent = iter Glv.perhaps_pull_exponent

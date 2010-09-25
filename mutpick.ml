(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * NOTE: rpost stands for rate-posterior
*)

let array_maxi a = 
  let best = ref 0.
  and besti = ref (-1)
  in
  for i=0 to (Array.length a)-1 do
    if a.(i) > !best then begin
      best := a.(i);
      besti := i; 
    end
  done;
  assert(-1 <> !besti);
  !besti

(* pick the ML state by taking the sum across rates for each state and site *)
let pick_of_rpost g = 
  let n_sites = Glv.get_n_sites g
  and n_states = Glv.get_n_states g
  and n_rates = Glv.get_n_rates g
  in
  let picks = Array.make n_sites (-1)
  and u = Array.make n_states 0.
  in
  for site=0 to n_sites-1 do
    Array.fill u 0 n_states 0.;
    for rate=0 to n_rates-1 do
      for state=0 to n_states-1 do
        u.(state) <- u.(state) +. (Glv.get_a ~rate ~site ~state g)
      done
    done;
    picks.(site) <- array_maxi u
  done;
  picks
  
let get_picks model t ~darr ~parr locs =
  let evolv = Glv.mimic (Glv_arr.get_one darr)
  and rpost = Glv.mimic (Glv_arr.get_one darr)
  in
  List.map
    (fun loc ->
      Glv.evolve_into model ~dst:evolv 
        ~src:(Glv_arr.arr_get darr loc) (Gtree.get_bl t loc);
      Glv.statd_pairwise_prod 
        model ~dst:rpost evolv (Glv_arr.arr_get parr loc);
      pick_of_rpost rpost)
    locs

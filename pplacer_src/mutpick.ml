(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * NOTE: rpost stands for rate-posterior
*)

open MapsSets

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

(* Get the most likely vector at a chosen node id.
 * atarr is the Glv_arr which gives the Glv on the side we are interested in,
 * while neigharr is the neighbor.
 * u1 and u2 are utility vectors like Glv.mimic (Glv_arr.get_one darr) *)
let get_pick u1 u2 model t ~atarr ~neigharr id =
  Glv.evolve_into model
    ~dst:u1
    ~src:(Glv_arr.arr_get neigharr id)
    (Gtree.get_bl t id);
  Glv.statd_pairwise_prod
    model ~dst:u2 u1 (Glv_arr.arr_get atarr id);
  pick_of_rpost u2

let get_pickpair u1 u2 model t ~darr ~parr id =
  let at_d = get_pick u1 u2 model t ~atarr:darr ~neigharr:parr id
  and at_p = get_pick u1 u2 model t ~atarr:parr ~neigharr:darr id
  in
  (at_d, at_p)

(* make a map from a list of edge ids to the most likely vectors on either side
 * of the edge: order is (distal, proximal) *)
let pickpair_map model t ~darr ~parr ids =
  let u1 = Glv.mimic (Glv_arr.get_one darr)
  and u2 = Glv.mimic (Glv_arr.get_one darr) in
  List.fold_right
    (fun id ->
      IntMap.add id (get_pickpair u1 u2 model t ~darr ~parr id))
    ids
    IntMap.empty

(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open Fam_batteries
open MapsSets

let min_width = 0.5


let vec_entropy v = 
  (* make sure that it's a probability vector *)
  assert(1e-15 > 
    abs_float(1. -. (Fam_gsl_matvec.vecFold_left ( +. ) 0. v)));
  Fam_gsl_matvec.vecFold_left
    (fun accu p ->
      assert(p >= 0. && p <= 1.);
      accu -. (if p = 0. then 0. else p *. (log p)))
    0.
    v

(* for a given 
 * for each site, we take the entropy of the normalized 
 *)
let glv_entropy statd x_glv y_glv = 
  let n_rates = Glv.n_rates x_glv 
  and n_states = Gsl_vector.length statd in
  let dest_site = Gsl_vector.create n_states
  and tot_site = Gsl_vector.create n_states
  in
  ArrayFuns.fold_left2 (* fold over sites *)
    (fun tot_entropy x_site y_site -> 
      Gsl_vector.set_zero tot_site;
      for i=0 to n_rates-1 do
        Linear.triplewise_prod 
          dest_site x_site.(i) y_site.(i) statd n_states;
        (* this is tot_site += dest_site *)
        Gsl_vector.add tot_site dest_site;
      done;
      Fam_gsl_matvec.l1_normalize tot_site;
      tot_entropy +. vec_entropy tot_site)
    0.
    x_glv
    y_glv

let make_entropy_map statd ~halfd ~halfp locs = 
  List.fold_right
    (fun id ->
      IntMap.add
        id
        (glv_entropy 
          statd 
          (IntMap.find id halfd) 
          (IntMap.find id halfp)))
    locs
    IntMap.empty

let entropy_tree scale_factor decor_ref_tree entropy_map =
  Decor_gtree.add_decor_by_map
    decor_ref_tree
    (IntMap.map
      (fun h -> [ Decor.width (min_width +. (scale_factor *. h)); ])
      entropy_map)

let write_entropy_tree scale_factor fname_base decor_ref_tree entropy_map = 
  Phyloxml.tree_to_file
    (entropy_tree scale_factor decor_ref_tree entropy_map)
    (fname_base^".entropy.xml") 


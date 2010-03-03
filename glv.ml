(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * this is a module for a generalized likelihood vector, which is
 * actually an across-site collection of across-rate collections of likelihood
 * vectors.
 * this abstraction layer means that we should be able to treat any glv like it
 * was a simple likelihood vector.
 *
 * the implementation is as an array (over sites) of Glv_sites.
 *
 * i wanted originally to strictly conform to a very general module signature
 * which would be universal across all types of likelihood type vectors.
 * however, it is necessary to pass some rate information in functions like
 * evolve_into. i think i could have done so by specifying some
 * extra_information type in the signature, but it didn't seem worth it given
 * that i don't know what other sorts of data i would want to support.
 *)

open Fam_batteries
open MapsSets

type glv = Glv_site.glv_site array

let make ~n_sites ~n_rates ~n_states = 
  Array.init
    n_sites
    (fun _ -> Glv_site.make ~n_rates ~n_states)

let make_init e_init init ~n_sites ~n_rates ~n_states = 
  Array.init
    n_sites
    (fun _ -> Glv_site.make_init e_init init ~n_rates ~n_states)
  
(* deep copy *)
let copy = Array.map Glv_site.copy

(* make a glv of the same dimensions *)
let mimic = Array.map Glv_site.mimic

let iter = Array.iter

(* set all of the entries of the glv to some float *)
let set_exp_and_all_entries g e x = 
  iter (fun s -> Glv_site.set_exp_and_all_entries s e x) g

(* mask does *not* copy anything, just gets a subarray *)
let mask site_mask_arr g = 
  let len = Array.length g in
  assert(len = Array.length site_mask_arr);
  let rec aux sofar i = 
    if i < 0 then sofar
    else
      if site_mask_arr.(i) then 
        aux (g.(i)::sofar) (i-1)
      else
        aux sofar (i-1)
  in
  Array.of_list (aux [] (len-1))

(* this is used when we want to make a glv out of a list of likelihood vectors.
 * differs from below because we want to make a new one.
 * *)
let lv_list_to_constant_rate_glv n_rates lv_list = 
  Array.map 
    (Glv_site.constant_rate_of_lv n_rates)
    (Array.of_list lv_list)

(* this is used when we have a pre-allocated GLV and want to fill it with a
 * same-length lv array *)
let prep_constant_rate_glv_from_lv_arr glv lv_arr = 
  assert(Array.length glv = Array.length lv_arr);
  ArrayFuns.iter2 Glv_site.set_all_lvs_exp_zero glv lv_arr

(* these assume that the GLV is reasonably healthy *)
let n_states g = assert(g <> [||]); Glv_site.n_states g.(0)
let n_rates g = assert(g <> [||]); Glv_site.n_rates g.(0)
let n_sites g = Array.length g

let ppr = Ppr.ppr_array Glv_site.ppr


(* *** pulling exponent *** *)

let perhaps_pull_exponent g = 
  iter Glv_site.perhaps_pull_exponent g

(* *** likelihood calculations *** *)

(* log_like2_statd:
 * take the log like of the product of two things then dot with the stationary
 * distribution. there are lots of things we don't do error checking
 * on. *)
let log_like2_statd model x_glv y_glv = 
  assert(n_rates x_glv = n_rates y_glv);
  let fn_rates = float_of_int (n_rates x_glv) in
  let statd = Model.statd model in
  let our_n_states = Gsl_vector.length statd in
  ArrayFuns.fold_left2 (* fold over sites *)
   (fun site_tot x_s y_s -> 
     site_tot +. 
     (Glv_site.log_like2_statd 
        statd fn_rates our_n_states x_s y_s))
   0. x_glv y_glv

(* log_like3_statd:
 * take the log like of the product of three things then dot with the stationary
 * distribution. there are lots of things we don't do error checking
 * on. *)
let log_like3_statd model x_glv y_glv z_glv = 
  assert(n_rates x_glv = n_rates y_glv &&
         n_rates y_glv = n_rates z_glv);
  let fn_rates = float_of_int (n_rates x_glv) in
  let statd = Model.statd model in
  let our_n_states = Gsl_vector.length statd in
  ArrayFuns.fold_left3 (* fold over sites *)
   (fun site_tot x_s y_s z_s -> 
     site_tot +. 
     (Glv_site.log_like3_statd 
        statd fn_rates our_n_states x_s y_s z_s))
   0. x_glv y_glv z_glv

(* evolve_into:
 * evolve src according to model for branch length bl, then store the
 * results in dst.
 *)
let evolve_into model ~dst ~src bl = 
  (* prepare the matrices in our matrix cache *)
  Model.prep_mats_for_bl model bl;
  (* iter over sites *)
  ArrayFuns.iter2 
    (fun dest_site src_site ->
      Glv_site.evolve_into model ~dst:dest_site ~src:src_site)
    dst
    src;
  ()

(* functional version *)
let evolve model src_glv bl = 
  let dest_glv = copy src_glv in
  evolve_into model ~dst:dest_glv ~src:src_glv bl;
  dest_glv

(* copy src to dest *)
let memcpy ~src ~dst = 
  ArrayFuns.iter2 
    (fun s_src s_dst -> Glv_site.memcpy ~src:s_src ~dst:s_dst)
    src dst

(* take the pairwise product of glvs g1 and g2, then store in dest. *)
let pairwise_product ~dst g1 g2 = 
  let n_states = n_states dst in
  ArrayFuns.iter3
    (fun s_dst s1 s2 ->
      Glv_site.pairwise_product ~dst:s_dst s1 s2 n_states)
    dst g1 g2

(* take the product of all of the GLV's in the list, then store in dst. 
 * could probably be implemented more quickly, but typically we are only taking
 * pairwise products anyway. we pull out the x::y below to optimize for that
 * case. *)
let listwise_product dst = function
  | x::y::rest ->
      (* first product of first two *)
      pairwise_product ~dst x y;
      (* now take product with each of the rest *)
      List.iter (pairwise_product ~dst dst) rest
  | [src] -> 
      (* just copy over *)
      memcpy ~dst ~src
  | [] -> assert(false)

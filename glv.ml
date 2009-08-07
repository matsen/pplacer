(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

(* glv.ml
 * this is a module for a generalized likelihood vector, which is
 * actually an across-site collection of across-rate collections of likelihood
 * vectors.
 * this abstraction layer means that we should be able to treat any glv like it
 * was a simple likelihood vector.
 *
 * the implementation is as a list (over sites) of vector lists (over rates).
 *
 * note that i wanted originally to strictly conform to a very general module
 * signature which would be universal across all types of likelihood type
 * vectors. however, it is necessary to pass some rate information in functions
 * like evolve_into. i think i could have done so by specifying some
 * extra_information type in the signature, but it didn't seem worth it given
 * that i don't know what other sorts of data i would want to support.
 *
 *)

open Fam_batteries
open MapsSets
open Model

type glv = Gsl_vector.vector list list

let of_like_vect x = [[x]]
let copy = List.map (List.map Gsl_vector.copy)
let zero n_sites n_rates n_states = 
  ListFuns.init n_sites (fun _ -> 
    ListFuns.init n_rates (fun _ -> 
      Gsl_vector.create ~init:0. n_states))

(* these assume that the GLV is reasonably healthy *)
let n_states g = assert(g <> [] && (List.hd g) <> []); 
                 Gsl_vector.length (List.hd (List.hd g))
let n_rates g = assert(g <> []); List.length (List.hd g)
let n_sites g = List.length g

(* the following two are key to making real glv's *)
let combine_over_sites = ( @ )
let combine_over_rates ll1 ll2 = List.map2 ( @ ) ll1 ll2

(* this is used when we want to make a glv out of a list of likelihood vectors
 * *)
let lv_list_to_constant_rate_glv n_rates lv_list = 
  List.map
    (fun lv ->
      ListFuns.init n_rates (fun _ -> lv))
    lv_list

let ppr_glv = Ppr.ppr_list (Ppr.ppr_list Fam_gsl_matvec.ppr_gsl_vector)
let ppr_glv_intmap = IntMapFuns.ppr_gen ppr_glv

(* *** likelihood calculations *** *)

(* we "clean up" like so because an infinite value passed to a GSL function will
 * throw an exception. it would be nice if we could do better, as this probably
 * throws off the Brent method. *)
let finite_infinity x = 
  match Pervasives.classify_float x with
  | FP_infinite -> -. max_float
  | FP_nan -> -. max_float
  | _ -> x

  (*
(* log_like2:
 * take the inner product of two things then dot with the stationary
 * distribution. there are lots of things we don't do error checking on. *)
let log_like2 model x_glv y_glv = 
  assert(n_rates x_glv = n_rates y_glv);
  let fn_rates = float_of_int (n_rates x_glv) in
  let statd = model.statd in
  let size = Gsl_vector.length statd in
  finite_infinity 
    (List.fold_left2 (* fold over sites *)
      (fun site_tot x_site y_site -> 
        site_tot +. (* total is product in log world *)
          log (List.fold_left2 (* fold over rates *)
            (fun rate_tot x_lv y_lv -> 
              rate_tot+.(Base.triple_dot statd x_lv y_lv size))
            0. x_site y_site) /. fn_rates)
      0. x_glv y_glv)
    *)

(* log_like3:
 * take the inner product of three things then dot with the stationary
 * distribution. there are lots of things we don't do error checking on. *)
let log_like3 model x_glv y_glv z_glv = 
  assert(n_rates x_glv = n_rates y_glv &&
         n_rates y_glv = n_rates z_glv);
  let fn_rates = float_of_int (n_rates x_glv) in
  let statd = model.statd in
  let size = Gsl_vector.length statd in
  finite_infinity 
    (ListFuns.fold_left3 (* fold over sites *)
      (fun site_tot x_site y_site z_site -> 
        site_tot +. (* total is product in log world *)
          (log ((ListFuns.fold_left3 (* fold over rates *)
            (fun rate_tot x_lv y_lv z_lv -> 
              rate_tot+.(Base.quad_dot statd x_lv y_lv z_lv size))
            0. x_site y_site z_site) /. fn_rates)))
      0. x_glv y_glv z_glv)


(* make_evolve_mats:
 * make matrices for each rate
 *)
let make_evolve_mats model bl = 
  List.map 
    (fun rate -> (model.diagdq)#expWithT (bl *. rate)) 
    model.rates 

(* evolve_into:
 * evolve src_glv according to model for branch length bl, then store the
 * results in dest_glv.
 *)
let evolve_into model dest_glv src_glv bl = 
  let evolve_mats = make_evolve_mats model bl in
  List.iter2 (* iter over sites *)
    (fun dest_site src_site ->
      ListFuns.iter3 (* iter over rates *)
        (fun dest_lv src_lv mat -> 
          Fam_gsl_matvec.matVecMul dest_lv mat src_lv)
        dest_site
        src_site
        evolve_mats)
    dest_glv
    src_glv;
  ()


(* evolve_site_lvs_into:
 * evolve site_lvs according to model for each rate, then store the results in
 * dest_glv. see intro of this file about site_lvs.
 *)
let evolve_site_lvs_into model dest_glv site_lvs bl = 
  let evolve_mats = make_evolve_mats model bl in
  List.iter2 (* iter over sites *)
    (fun dest_site src_site ->
      List.iter2 (* iter over rates *)
        (fun dest_lv mat -> 
          Fam_gsl_matvec.matVecMul dest_lv mat src_site)
        dest_site
        evolve_mats)
    dest_glv
    site_lvs;
  ()



(* pairwise_product:
 * take the pairwise product of glvs g1 and g2, then store in dest. *)
let pairwise_product dest g1 g2 = 
  ListFuns.iter3
    (fun site_dest site_g1 site_g2 ->
      ListFuns.iter3 
        (fun rate_dest rate_g1 rate_g2 ->
          Base.pairwise_prod rate_dest rate_g1 rate_g2)
        site_dest site_g1 site_g2)
    dest g1 g2

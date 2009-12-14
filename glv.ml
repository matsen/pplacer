(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * this is a module for a generalized likelihood vector, which is
 * actually an across-site collection of across-rate collections of likelihood
 * vectors.
 * this abstraction layer means that we should be able to treat any glv like it
 * was a simple likelihood vector.
 *
 * the implementation is as an array (over sites) of vector arrays (over rates).
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
open Model

let arr_get = Array.get

type glv = Gsl_vector.vector array array


let make ~n_sites ~n_rates ~n_states = 
  Array.init
    n_sites
    (fun _ -> 
      Array.init n_rates (fun _ -> Gsl_vector.create n_states))

let make_init init ~n_sites ~n_rates ~n_states = 
  Array.init
    n_sites
    (fun _ -> 
      Array.init n_rates (fun _ -> Gsl_vector.create ~init n_states))
  
(* HERE: this can disappear after migrating to glv_arr *)
let make_empty ~n_sites ~n_rates = 
  Array.make_matrix n_sites n_rates (Gsl_vector.create 0)

(* deep copy *)
let copy = Array.map (Array.map Gsl_vector.copy)

(* make a glv of the same dimensions *)
let mimic = Array.map (Array.map Fam_gsl_matvec.vecMimic)

let get ~site ~rate g = arr_get (arr_get g site) rate
let set g ~site ~rate lv = g.(site).(rate) <- lv

(* iter over the likelihood vectors *)
let lv_iter f g = 
  Array.iter (Array.iter f) g

(* set all of the entries of the glv to some float *)
let set_all_entries g x = 
  lv_iter (fun v -> Gsl_vector.set_all v x) g

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
    (fun lv ->
      Array.init n_rates (fun _ -> lv))
    (Array.of_list lv_list)

(* this is used when we have a pre-allocated GLV and want to fill it with a
 * same-length lv array *)
let prep_constant_rate_glv_from_lv_arr glv lv_arr = 
  assert(Array.length glv = Array.length lv_arr);
  ArrayFuns.iter2
    (fun site src -> 
      Array.iter
        (fun rate_site -> Gsl_vector.memcpy ~dst:rate_site ~src)
        site)
    glv
    lv_arr

(* these assume that the GLV is reasonably healthy *)
let n_states g = assert(g <> [||] && g.(0) <> [||]); 
                 Gsl_vector.length (g.(0).(0))
let n_rates g = assert(g <> [||]); Array.length (g.(0))
let n_sites g = Array.length g

let ppr_glv = Ppr.ppr_array (Ppr.ppr_array Fam_gsl_matvec.ppr_gsl_vector)
let ppr_glv_intmap = IntMapFuns.ppr_gen ppr_glv (* HERE *)

(* *** likelihood calculations *** *)

(* we "clean up" like so because an infinite value passed to a GSL function will
 * throw an exception. it would be nice if we could do better, as this probably
 * throws off the Brent method. *)
let finite_infinity x = 
  match Pervasives.classify_float x with
  | FP_infinite -> -. max_float
  | FP_nan -> -. max_float
  | _ -> x


(* log_like3_statd:
 * take the log like of the product of three things then dot with the stationary
 * distribution. there are lots of things we don't do error checking
 * on. *)
let log_like3_statd model x_glv y_glv z_glv = 
  assert(n_rates x_glv = n_rates y_glv &&
         n_rates y_glv = n_rates z_glv);
  let fn_rates = float_of_int (n_rates x_glv) in
  let statd = model.statd in
  let size = Gsl_vector.length statd in
  finite_infinity 
    (ArrayFuns.fold_left3 (* fold over sites *)
      (fun site_tot x_site y_site z_site -> 
        site_tot +. (* total is product in log world *)
          (log ((ArrayFuns.fold_left3 (* fold over rates *)
            (fun rate_tot x_lv y_lv z_lv -> 
              rate_tot+.(Linear.quad_dot statd x_lv y_lv z_lv size))
            0. x_site y_site z_site) /. fn_rates)))
      0. x_glv y_glv z_glv)


(* make_evolve_mats:
 * make matrices for each rate
 *)
let make_evolve_mats model bl = 
  Array.map 
    (fun rate -> (model.diagdq)#expWithT (bl *. rate)) 
    model.rates 

(* evolve_into:
 * evolve src_glv according to model for branch length bl, then store the
 * results in dest_glv.
 * HERE: do i want to make it faster by avoiding allocation for matrices?
 *)
let evolve_into model ~dst ~src bl = 
  let evolve_mats = make_evolve_mats model bl in
  ArrayFuns.iter2 (* iter over sites *)
    (fun dest_site src_site ->
      ArrayFuns.iter3 (* iter over rates *)
        (fun dest_lv src_lv mat -> 
          Fam_gsl_matvec.matVecMul dest_lv mat src_lv)
        dest_site
        src_site
        evolve_mats)
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
    (fun site_src site_dest ->
      ArrayFuns.iter2
        (fun rate_src rate_dest ->
          Gsl_vector.memcpy ~src:rate_src ~dst:rate_dest)
        site_src site_dest)
    src dst

(* take the pairwise product of glvs g1 and g2, then store in dest. *)
let pairwise_product ~dst g1 g2 = 
  let n_states = n_states dst in
  ArrayFuns.iter3
    (fun site_dest site_g1 site_g2 ->
      ArrayFuns.iter3 
        (fun rate_dest rate_g1 rate_g2 ->
          Linear.pairwise_prod rate_dest rate_g1 rate_g2 n_states)
        site_dest site_g1 site_g2)
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



(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * a module for site likelihoods.
 *
 * this is designed to avoid underflows in most cases when doing phylogenetics
 * calculation, although such behavior is not guaranteed.
 *
 * the key detail is that a base-two exponent is stored in the e field.
 * "pulling" the exponent refers to finding a suitable exponent x and then
 * dividing all of the entries of the site likelihood vectors by 2^x to bring
 * them back in line, then storing x in field e.
 *
 * the exponent x is chosen to be the maximum (over rates) of the maximum (over
 * entries) of the site_glv. these maximums will be the dominant likelihood
 * vector entries in the glv, and the most important ones to protect.
 *)

open Fam_batteries
open MapsSets

(* below 2^-50 we pull out the exponent into the int *)
let min_allowed_twoexp = -50

let log_of_2 = log 2.

type glv_site = { e: int ref; a : Gsl_vector.vector array; }

let get_e s = !(s.e)
let set_e s e = s.e := e
let get_a s = s.a

let make ~n_rates ~n_states = 
  { e = ref 0;
  a = Array.init n_rates (fun _ -> Gsl_vector.create n_states); }

let make_init e_init init ~n_rates ~n_states = 
  { e = ref e_init;
  a = Array.init n_rates (fun _ -> Gsl_vector.create ~init n_states) }
  
(* deep copy *)
let copy s = { e = ref (get_e s); 
               a = Array.map Gsl_vector.copy s.a; }

(* make a glv of the same dimensions *)
let mimic s = { e = ref 0; 
                a = Array.map Fam_gsl_matvec.vecMimic s.a; }

(* iter over the likelihood vectors *)
let lv_iter f s = 
  Array.iter f (get_a s)

(* set all of the entries of the glv_site to some float *)
let set_exp_and_all_entries s e x = 
  set_e s e;
  lv_iter (fun v -> Gsl_vector.set_all v x) s

(* make a constant rate lv_site from a lv *)
let constant_rate_of_lv n_rates lv =
  { e = ref 0; a = Array.init n_rates (fun _ -> lv); }

(* this is used when we have a pre-allocated GLV and want to fill it with a
 * same-length lv array. at the same time, set the exponent to zero. *)
let set_all_lvs_exp_zero s lv = 
  Array.iter (fun ra -> Gsl_vector.memcpy ~dst:ra ~src:lv) (get_a s);
  set_e s 0

(* these assume that the GLV is reasonably healthy *)
let n_states s = assert(s.a <> [||]); 
                 Gsl_vector.length ((get_a s).(0))
let n_rates s = Array.length (get_a s)

let ppr ff s = 
  Format.fprintf ff
    "1e%d@ %a" 
    (get_e s) 
    (Ppr.ppr_array Fam_gsl_matvec.ppr_gsl_vector) (get_a s)

(* *** pulling exponent *** *)

(* gets the base two exponent *)
let get_twoexp x = snd (frexp x)

(* makes a float given a base two exponent. we use 0.5 because:
# frexp (ldexp 1. 3);;
- : float * int = (0.5, 4)
so that's how ocaml interprets 2^i anyway.
*)
let of_twoexp i = ldexp 0.5 (i+1)

(* find the maximum 2-exponent of a vector *)
let max_max_twoexp s = 
  let max_max = ref (-1024) in
  let our_n_states = n_states s in
  lv_iter 
    (fun v -> 
      for i=0 to (our_n_states-1) do
        let (_, twoexp) = frexp (Bigarray.Array1.unsafe_get v i) in
        if twoexp > !max_max then max_max := twoexp
      done;)
    s;
  !max_max

(* pull out the exponent if it's below min_allowed_twoexp *)
let perhaps_pull_exponent s = 
  let max_max = max_max_twoexp s in
  if max_max < min_allowed_twoexp then begin
    (* take the negative so that we "divide" by 2^max_max *)
    let normalizer = of_twoexp (-max_max) in
    lv_iter (fun v -> Gsl_vector.scale v normalizer) s;
    (* update the exponent with max_max *)
    set_e s ((get_e s) + max_max);
  end


(* *** likelihood calculations *** *)

(* we "clean up" like so because an infinite value passed to a GSL function will
 * throw an exception. it would be nice if we could do better, as this probably
 * throws off the Brent method. *)
let finite_log x = 
  let log_x = log x in
  match Pervasives.classify_float log_x with
  | FP_infinite | FP_nan -> -. 1e200
  | _ -> log_x

(* the log like of the "product" of two things *)
let log_like2_statd statd fn_rates our_n_states x_s y_s = 
  (float_of_int ((get_e x_s) + (get_e y_s)))
    *. log_of_2 +.
    finite_log ((ArrayFuns.fold_left2 (* fold over rates *)
      (fun rate_tot x_lv y_lv -> 
        rate_tot+.(Linear.triple_dot statd x_lv y_lv our_n_states))
      0. (get_a x_s) (get_a y_s)) /. fn_rates)

(* the log like of the "product" of three things *)
let log_like3_statd statd fn_rates our_n_states x_s y_s z_s = 
  (float_of_int ((get_e x_s) + (get_e y_s) + (get_e z_s)))
    *. log_of_2 +.
    finite_log ((ArrayFuns.fold_left3 (* fold over rates *)
      (fun rate_tot x_lv y_lv z_lv -> 
        rate_tot+.(Linear.quad_dot statd x_lv y_lv z_lv our_n_states))
      0. (get_a x_s) (get_a y_s) (get_a z_s)) /. fn_rates)


(* evolve_into:
 * evolve src according to model for branch length bl, then store the
 * results in dst.
 * Note: one must call Model.prep_mats_for_bl before calling this subroutine so
 * that the matrices are properly prepared.
 *)
let evolve_into model ~dst ~src = 
  ArrayFuns.iter3 (* iter over rates *)
    (fun dest_lv src_lv mat -> 
      Fam_gsl_matvec.matVecMul dest_lv mat src_lv)
    (get_a dst)
    (get_a src)
    (Model.mats model);
  (* just move the exponent over *)
  set_e dst (get_e src);
  ()

(* copy src to dest *)
let memcpy ~src ~dst = 
   ArrayFuns.iter2
     (fun r_src r_dst -> Gsl_vector.memcpy ~src:r_src ~dst:r_dst)
     (get_a src) (get_a dst);
   set_e dst (get_e src)

(* take the pairwise product of glvs g1 and g2, then store in dest. *)
let pairwise_product ~dst s1 s2 n_states = 
  ArrayFuns.iter3 
    (fun r_dest r1 r2 ->
      Linear.pairwise_prod r_dest r1 r2 n_states)
    (get_a dst) (get_a s1) (get_a s2);
  set_e dst ((get_e s1) + (get_e s2))


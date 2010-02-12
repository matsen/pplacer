(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * a type for three-taxon trees which is useful for likelihood calculations. 
 * we assume that the distance from the dist do the prox is fixed. see
 * set_dist_bl.
 *)

open MapsSets
open Fam_batteries

let n_like_calls = ref 0

type three_tax = { 
  model  : Model.model;
  prox   : Glv_edge.glv_edge;      (* the proximal glv *)
  dist   : Glv_edge.glv_edge;      (* the distal glv *)
  pend  : Glv_edge.glv_edge;      (* the pendant, i.e. query glv *)
}

let get_pend_bl tt = Glv_edge.get_bl tt.pend
let get_dist_bl tt = Glv_edge.get_bl tt.dist
let get_prox_bl tt = Glv_edge.get_bl tt.prox
let get_cut_bl tt = (get_dist_bl tt) +. (get_prox_bl tt)

let make model ~prox ~dist ~pend = 
  { model  = model;
    prox   = prox;
    dist   = dist;
    pend  = pend;
  }

let log_like tt = 
  Glv.log_like3_statd
    tt.model 
    (Glv_edge.get_evolv tt.prox) 
    (Glv_edge.get_evolv tt.dist)
    (Glv_edge.get_evolv tt.pend) 
    
let set_pend_bl tt pend_bl = 
  Glv_edge.set_bl tt.model tt.pend pend_bl

let set_dist_bl tt dist_bl = 
  let prox_bl = (get_cut_bl tt) -. dist_bl in
  assert(prox_bl >= 0.);
  Glv_edge.set_bl tt.model tt.dist dist_bl;
  Glv_edge.set_bl tt.model tt.prox prox_bl

(* we minimize the negative of the log likelihood *)
let optimize_something tolerance set_fun start_value max_value tt = 
  let opt_fun value = 
    incr n_like_calls;
    set_fun value;
    -. (log_like tt)
  in
  Minimization.brent 
      opt_fun start_value 0. max_value tolerance

let optimize_pend_bl tolerance max_value tt = 
  optimize_something 
    tolerance (set_pend_bl tt) (get_pend_bl tt) max_value tt 

let optimize_dist_bl tolerance tt = 
  optimize_something 
    tolerance (set_dist_bl tt) (get_dist_bl tt) (get_cut_bl tt) tt 

let optimize tolerance max_pend_bl max_iter tt = 
  let rec aux which_step prev_pend prev_dist = 
    if which_step > max_iter then
      raise Minimization.ExceededMaxIter;
    let curr_pend = optimize_pend_bl tolerance max_pend_bl tt
    and curr_dist = optimize_dist_bl tolerance tt in
    if (abs_float (prev_pend -. curr_pend) > tolerance ||
        abs_float (prev_dist -. curr_dist) > tolerance)
      then aux (which_step+1) curr_pend curr_dist
      else ()
  in
  n_like_calls := 0;
  let () = aux 1 (get_pend_bl tt) (get_dist_bl tt) in
  !n_like_calls

let get_results tt = (log_like tt, get_pend_bl tt, get_dist_bl tt)

let copy_bls ~src ~dest = 
  set_pend_bl dest (get_pend_bl src);
  set_dist_bl dest (get_dist_bl src)

(* write out the likelihood surface, with base_ll discounted. this is what
 * integrate actually integrates, sampled on an integer-plus-half lattice. *)
let write_like_surf prior_fun max_pend tt fname n_samples =
  let base_ll = log_like tt 
  and cut_bl = get_cut_bl tt in
  let dist_incr = cut_bl /. (float_of_int n_samples)
  and pend_incr = max_pend /. (float_of_int n_samples) 
  and float_and_half i = 0.5 +. (float_of_int i) 
  and ch = open_out fname
  in
  for i=0 to n_samples-1 do
    set_dist_bl tt (dist_incr *. (float_and_half i));
    for j=0 to n_samples-1 do
      let pend_bl = pend_incr *. (float_and_half j) in
      set_pend_bl tt pend_bl;
      Printf.fprintf ch "%g\t"
        ((exp ((log_like tt) -. base_ll)) *. (prior_fun pend_bl));
    done;
    Printf.fprintf ch "\n";
  done;
  close_out ch

(* find an appropriate upper limit for pendant branch length integration. if we
 * come up with a resonable upper limit then we get better results than
 * integrating out to max_pend. 
 * this function uses the current pend branch length and moves right in
 * increments of the current branch length, stopping when the ll function drops
 * below 1e-10*orig_ll.
 * note 1: this does change the pend_bl.
 * note 2: this assumes that the likelihood function is monotonic in the pendant
 * branch length.
 *)
let find_upper_limit max_pend orig_ll tt = 
  let orig_pend_bl = get_pend_bl tt 
  and min_ll = 1e-10 *. orig_ll
  in
  let rec aux next_pend_bl = 
    set_pend_bl tt next_pend_bl;
    if min_ll > log_like tt then get_pend_bl tt
    else if next_pend_bl > max_pend then max_pend
    else aux (orig_pend_bl +. next_pend_bl)
  in
  aux (get_pend_bl tt)

(* the idea here is to properly integrate log likelihood functions by removing
 * some portion so that when we actually do the integration, we don't have
 * underflow problems. 
 * we use the original branch lengths in tt give us a baseLL.
 * this calculates then
 * baseLL + \log ( cut_bl^{-1} \int \int \exp(llF - baseLL) * prior(x) dx dy )
 * Note: modifies the branch lengths in tt!
*)
let calc_marg_prob prior_fun rel_err max_pend tt =
  let abs_err = 0. in (* do not specify an absolute error *)
  (* first calculate a base_ll. we use the given base_pend and the midpoint of
   * the edge *)
  let base_ll = log_like tt 
  and cut_bl = get_cut_bl tt in
  let upper_limit = find_upper_limit max_pend base_ll tt in
  try
    base_ll +. 
      log 
        ((Integration.value_integrate 
          (fun dist_bl -> 
            set_dist_bl tt dist_bl;
            Integration.value_integrate 
              (fun pend_bl -> 
                set_pend_bl tt pend_bl;
                (exp ((log_like tt) -. base_ll)) 
                  *. (prior_fun pend_bl))
              0. upper_limit ~abs_err ~rel_err)
          0. cut_bl ~abs_err ~rel_err)
        /. cut_bl)
        (* normalize out the integration over a branch length *) 
  with
  | Gsl_error.Gsl_exn(error_num, error_str) ->
      if error_num = Gsl_error.ETOL then begin
(* Integration failed to reach tolerance with highest-order rule *)
        Printf.printf "Warning: %s\n" error_str;
(* return the base LL *)
        base_ll
      end
      else
        raise (Gsl_error.Gsl_exn(error_num, error_str))


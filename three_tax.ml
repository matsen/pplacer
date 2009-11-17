(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * a type for three-taxon trees which is useful for likelihood calculations. 
 * we assume that the distance from the dist do the prox is fixed. see
 * set_dist_bl.
 *)

open MapsSets
open Fam_batteries

type three_tax = { 
  model  : Model.model;
  prox   : Glv_edge.glv_edge;      (* the proximal glv *)
  dist   : Glv_edge.glv_edge;      (* the distal glv *)
  query  : Glv_edge.glv_edge;      (* the query glv *)
}

let get_query_bl tt = Glv_edge.get_bl tt.query
let get_dist_bl tt = Glv_edge.get_bl tt.dist
let get_prox_bl tt = Glv_edge.get_bl tt.prox
let get_cut_bl tt = (get_dist_bl tt) +. (get_prox_bl tt)

let make model ~prox ~dist ~query = 
  { model  = model;
    prox   = prox;
    dist   = dist;
    query  = query;
  }

let log_like tt = 
  Glv.log_like3_statd
    tt.model 
    (Glv_edge.get_evolv tt.prox) 
    (Glv_edge.get_evolv tt.dist)
    (Glv_edge.get_evolv tt.query) 
    
let set_query_bl tt query_bl = 
  Glv_edge.set_bl tt.model tt.query query_bl

let set_dist_bl tt dist_bl = 
  let prox_bl = (get_cut_bl tt) -. dist_bl in
  assert(prox_bl >= 0.);
  Glv_edge.set_bl tt.model tt.dist dist_bl;
  Glv_edge.set_bl tt.model tt.prox prox_bl

(* we minimize the negative of the log likelihood *)
let optimize_something tolerance set_fun start_value max_value tt = 
  let opt_fun value = 
    set_fun value;
    -. (log_like tt)
  in
  Minimization.brentOptimization 
      opt_fun start_value 0. max_value tolerance

let optimize_query_bl tolerance max_value tt = 
  optimize_something 
    tolerance (set_query_bl tt) (get_query_bl tt) max_value tt 

let optimize_dist_bl tolerance tt = 
  optimize_something 
    tolerance (set_dist_bl tt) (get_dist_bl tt) (get_cut_bl tt) tt 

let optimize tolerance max_query_bl max_iter tt = 
  let rec aux which_step prev_query prev_dist = 
    if which_step > max_iter then
      raise Minimization.ExceededMaxIter;
    let curr_query = optimize_query_bl tolerance max_query_bl tt
    and curr_dist = optimize_dist_bl tolerance tt in
    if (abs_float (prev_query -. curr_query) > tolerance ||
        abs_float (prev_dist -. curr_dist) > tolerance)
      then aux (which_step+1) curr_query curr_dist
      else ()
  in
  aux 1 (get_query_bl tt) (get_dist_bl tt)

let get_results tt = (log_like tt, get_query_bl tt, get_dist_bl tt)

let copy_bls ~src ~dest = 
  set_query_bl dest (get_query_bl src);
  set_dist_bl dest (get_dist_bl src)

(* in two stage optimization, we first optimize the one-rate tt corresponding to
 * the dominant rate on the reference tree, then optimize the whole enchilada 
 *
let two_stage_optimize tolerance max_query_bl max_iter tt = 

  first optimize with respect to the dominant rate, then optimize with the whole
  thing
 * *)
 

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
  try
    base_ll +. 
      log 
        ((Integration.value_integrate 
          (fun dist_bl -> 
            set_dist_bl tt dist_bl;
            Integration.value_integrate 
              (fun pend_bl -> 
                set_query_bl tt pend_bl;
                (exp ((log_like tt) -. base_ll)) 
                  *. (prior_fun pend_bl))
              0. max_pend ~abs_err ~rel_err)
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
        

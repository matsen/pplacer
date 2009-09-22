(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Makes a type for three-taxon trees which is useful for likelihood
 * calculations. 
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
      failwith "exceeded number of iterations";
    let curr_query = optimize_query_bl tolerance max_query_bl tt
    and curr_dist = optimize_dist_bl tolerance tt in
    if (abs_float (prev_query -. curr_query) > tolerance ||
        abs_float (prev_dist -. curr_dist) > tolerance)
      then aux (which_step+1) curr_query curr_dist
      else ()
  in
  aux 1 (get_query_bl tt) (get_dist_bl tt)

let get_results tt = (log_like tt, get_query_bl tt, get_dist_bl tt)


(* the idea here is to properly integrate log likelihood functions by removing
 * some portion so that when we actually do the integration, we don't have
 * underflow problems. 
 * we use the original branch lengths in tt give us a baseLL.
 * this calculates then
 * baseLL + \log ( cut_bl^{-1} \int \int \exp(llF - baseLL) * prior(x) dx dy )
 * Note: modifies the branch lengths in tt!
*)
let calc_marg_prob prior_fun rel_err max_pend tt =
  let abs_err = 1000. in (* we don't worry about absolute error *)
  (* first calculate a base_ll. we use the given base_pend and the midpoint of
   * the edge *)
  let base_ll = log_like tt 
  and cut_bl = get_cut_bl tt in
  base_ll +. 
    log 
      ((Integration.valueIntegrate 
        (fun dist_bl -> 
          set_dist_bl tt dist_bl;
          Integration.valueIntegrate 
            (fun pend_bl -> 
              set_query_bl tt pend_bl;
              (exp ((log_like tt) -. base_ll)) 
                *. (prior_fun pend_bl))
            0. max_pend abs_err rel_err)
        0. cut_bl abs_err rel_err)
      /. cut_bl)
      (* normalize out the integration over a branch length *) 




(*
let locs = [0;1;2;3;4]
let it = Stree_io.of_newick_str "((x:0.2,y:3e-2):0.05,z:1e-5):0."
let mini = [| ("x","AA"); ("y","AT"); ("z","AA"); |]
let model = Model.build "GTR" false "JC.stats.txt" mini [1.; 2.]
let (d,p) = Rgma.dp_rgma_of_data model Alignment.Nucleotide_seq mini it [0;1;2;3;4]
let dm = Rgma.to_glv_map locs d
and pm = Rgma.to_glv_map locs p
let loc = 0
let cut_bl = 0.1
let query_glv = 
  Glv.lv_list_to_constant_rate_glv 
    (Model.n_rates model) 
    (List.map
      Nuc_models.likeArrOfNuc 
      (Array.to_list (StringFuns.to_char_array "CC")))
let start_pend = 0.2

let make_initial glv_map start_bl = 
  Glv_edge.make 
  model (IntMap.find loc glv_map) start_bl

let tt = 
  make 
    model
    ~dist:(make_initial dm (cut_bl /. 2.))
    ~prox:(make_initial pm (cut_bl /. 2.))
    ~query:(Glv_edge.make model query_glv start_pend)

let () = optimize 0.01 2. 100 tt
let q = tt
*)

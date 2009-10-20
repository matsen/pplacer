
(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 *)

open MapsSets
open Fam_batteries

exception Total_xi_not_zero of float

let rng = Gsl_rng.make Gsl_rng.MT19937

let standard_normal () = 
  Gsl_randist.gaussian rng ~sigma:1.

let collect_placement_info pl =
  List.map
    (function place ->
      (Placement.location place,
      Placement.distal_bl place))
    pl

let total_normals n_map = 
  let tot = ref 0. in
  IntMap.iter 
    (fun _ ->
      List.iter 
        (fun (_,norm_sample) ->
          tot := !tot +. norm_sample))
    n_map;
  !tot
 
(* we do a similar totaling thing as in the KR case, but different. 
 * the two components of the info vector are
 * norm_v.(0): the sum of the normal variables below
 * norm_v.(1): the proportion of the leaves below the edge
 * norm_v just means that it's the vector which is associated with the normal
 * simulation.
 *)
let normal_pair_approx n_samples ref_tree p pl1 pl2 = 
  let int_inv x = 1. /. (float_of_int x) in
  let all_pre_norm_map = 
    IntMap.map
      (List.sort compare) (* sort distal bl's along edge *)
      (IntMapFuns.of_pairlist_listly
        (collect_placement_info (pl1 @ pl2)))
  in
  let np1 = List.length pl1
  and np2 = List.length pl2 in
  let inv_np = int_inv (np1+np2) in
  let front_coeff = 
    sqrt (((int_inv np1) +. (int_inv np2)) *. inv_np) in
  (* decorate the map with normal distribution samples *)
  let sample_normals () = 
    IntMap.map 
      (List.map
        (fun distal ->
          (distal, standard_normal ())))
      all_pre_norm_map
  in
  (* go "past" a placement *)
  let update_norm_v norm_v sample = 
    norm_v.(0) <- norm_v.(0) +. sample;
    norm_v.(1) <- norm_v.(1) +. inv_np;
  in
  let starter_norm_v = [|0.; 0.|] in
  ListFuns.init 
    n_samples
    (fun _ -> 
      let n_map = sample_normals () in
      let norm_tot = total_normals n_map in
      let pre_xi_diff norm_v = 
        (* parens for emphasis *)
        norm_v.(0) -. (norm_v.(1) *. norm_tot) in 
      let to_xi_p p norm_v = 
        (abs_float (front_coeff *. (pre_xi_diff norm_v))) ** p in
      (* here we update the norm_v given an eta *)
      (* total across all of the edges of the tree *)
      let norm_edge_total n_map id = 
        Kr_distance.total_along_edge 
          (to_xi_p p)
          (Itree.get_bl ref_tree id)
          (Base.get_from_list_intmap id n_map)
          update_norm_v
      (* make sure that the kr_v totals to zero *)
      and check_final_norm_v final_norm_v = 
        let final_norm_diff = pre_xi_diff final_norm_v in
        if abs_float final_norm_diff > Kr_distance.tol then 
          raise (Total_xi_not_zero final_norm_diff)
      in
      (Kr_distance.total_over_tree 
        (norm_edge_total n_map)
        check_final_norm_v
        Mokaphy_base.v_list_sum
        (fun () -> Array.copy starter_norm_v)
        ref_tree)
      ** (Kr_distance.outer_exponent p))


(* make a distribution of distances via the normal approximation *)
let resampled_distn n_samples criterion p placerun1 placerun2 = 
  Placerun_distance.pair_dist_gen 
    (Pquery.best_place criterion)
    (normal_pair_approx n_samples)
    p 
    placerun1 
    placerun2

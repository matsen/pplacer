(* mokaphy v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * see scan: normal_approx.pdf
 *)

open MapsSets
open Fam_batteries

exception Avg_weight_not_one of float

(* these masses are used for downstream calculation *)
type labeled_mass = { pquery_num: int ;
                      mass: float }

(* the intermediate things held for calculation; see scan *)
type calc_intermediate = { omega: float ref;   (* weighted sum of normals *)
                           sigma: float ref; } (* sum of weights *)

let get_omega i = !(i.omega)
let get_sigma i = !(i.sigma)

let intermediate_sum i1 i2 = 
  { omega = ref ((get_omega i1) +. (get_omega i2));
    sigma = ref ((get_sigma i1) +. (get_sigma i2)) }

let intermediate_list_sum = 
  ListFuns.complete_fold_left intermediate_sum
                      
let normal_pair_approx rng weighting criterion n_samples p pr1 pr2 = 
  let np1 = Placerun.n_pqueries pr1
  and np2 = Placerun.n_pqueries pr2 in
  let int_inv x = 1. /. (float_of_int x) 
  and ref_tree = Placerun.get_same_tree pr1 pr2 in
  let labeled_mass_arr = Array.make (1+Gtree.top_id ref_tree) []
  and pquery_counter = ref 0
  and front_coeff = sqrt(int_inv(np1 * np2))
  and sample = Array.make (np1 + np2) 0. 
  in
  (* initialize the labeled_mass_arr array *)
  List.iter
    (fun pr ->
      List.iter
        (fun pquery ->
          (match weighting with
          | Mass_map.Weighted -> 
  (* this is not too elegant. because of roundoff in reading and writing the
   * placements, we have to re-normalize the masses so that we get a nice tidy
   * sum*)
              let pl = Pquery.place_list pquery in
              ListFuns.iter2
                (fun p mass ->
                  let edge_num = Placement.location p in
                  labeled_mass_arr.(edge_num) <-
                    (Placement.distal_bl p,
                    { pquery_num = !pquery_counter;
                    mass = mass })
                    :: (labeled_mass_arr.(edge_num)))
                pl
                (Base.normalized_prob (List.map criterion pl));
          | Mass_map.Unweighted -> 
              let p = Pquery.best_place criterion pquery in
              let edge_num = Placement.location p in
              labeled_mass_arr.(edge_num) <-
                (Placement.distal_bl p,
                { pquery_num = !pquery_counter;
                mass = 1. })
                :: (labeled_mass_arr.(edge_num)));
          incr pquery_counter)
        (Placerun.get_pqueries pr))
    [pr1; pr2];
  for i=0 to (Array.length labeled_mass_arr) - 1 do
    labeled_mass_arr.(i) <- 
      List.sort 
        (fun (a1,_) (a2,_) -> compare a1 a2)
        labeled_mass_arr.(i)
  done;
  (* the sampling routine *)
  let sample_normals () =
   Array.iteri
     (fun i _ -> sample.(i) <- Gsl_randist.gaussian rng ~sigma:1.)
     sample
  in
  (* go "past" a labeled mass *)
  let update_data data lm = 
    data.omega := (get_omega data) +. lm.mass *. sample.(lm.pquery_num);
    data.sigma := (get_sigma data) +. lm.mass;
  in
  (* take the samples and do the calculation *)
  ListFuns.init 
    n_samples
    (fun _ -> 
      sample_normals ();
      let sample_avg = 
        (int_inv (Array.length sample)) *. 
          (Array.fold_left (+.) 0. sample) in
      let to_xi_p p data = 
        (abs_float ((get_omega data) -. (get_sigma data) *. sample_avg)) ** p in
      (* total across all of the edges of the tree *)
      let edge_total id = 
        Kr_distance.total_along_edge 
          (to_xi_p p)
          (Gtree.get_bl ref_tree id)
          labeled_mass_arr.(id)
          update_data
      (* make sure that the average weight totals to one *)
      and check_final_data data = 
        let avg_weight = int_inv (np1 + np2) *. (get_sigma data) in
        if abs_float (avg_weight -. 1.) > Kr_distance.tol then
          raise (Avg_weight_not_one (avg_weight-.1.))
      in
      front_coeff *.
        (Kr_distance.total_over_tree 
          edge_total
          check_final_data
          intermediate_list_sum
          (fun () -> { omega = ref 0.; sigma = ref 0.; })
          ref_tree)
        ** (Kr_distance.outer_exponent p))

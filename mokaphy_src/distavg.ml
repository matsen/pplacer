(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * See pquery_distances.ml for some comments.
 *
 *)

open MapsSets
open Fam_batteries

type 'a data_t = 
  | Pairwise of 'a Placerun.t * 'a Placerun.t 
  | Single of 'a Placerun.t

let of_placerun_gen data weighting criterion exponent = 
  let total = ref 0. in
  let dist_fun = 
    (Pquery_distances.dist_fun_of_w weighting) criterion ca_info
  in
  let exp_dist_fun = 
    if exponent = 1. then dist_fun
    else (fun a b -> (dist_fun a b) ** exponent)
  in
  let total = ref 0. in
  let () = match data with
  | Pairwise (pr, pr') ->
      let tl = Gtree.tree_length (Placerun.get_ref_tree pr) in
      Base.list_iter_over_pairs_of_single 
        (fun a b -> total += !total +. (exp_dist_fun a b) /. tl)
        (Placerun.get_pqueries pr)
        (Placerun.get_pqueries pr')
  | Single pr ->
      let tl = Gtree.tree_length (Placerun.get_same_tree pr pr') in
      Base.list_iter_over_pairs_of_two 
        (fun a b -> total += !total +. (exp_dist_fun a b) /. tl)
        (Placerun.get_pqueries pr)
  in
  !total

let of_placerun_pair data weighting criterion exponent pr pr' = 
  of_placerun_gen (Pairwise (pr,pr')) weighting criterion exponent

let of_placerun data weighting criterion exponent pr = 
  of_placerun_gen (Single pr) weighting criterion exponent

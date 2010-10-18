(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * See pquery_distances.ml for some comments.
 *
 *)

open MapsSets
open Fam_batteries

type 'a data_t = 
  | Pairwise of 'a Placerun.placerun * 'a Placerun.placerun 
  | Single of 'a Placerun.placerun

let of_placerun_gen dist_fun data exponent = 
  let exp_dist_fun = 
    if exponent = 1. then dist_fun
    else (fun a b -> (dist_fun a b) ** exponent)
  in
  let total = ref 0. in
  let add_to_tot tl a b = total := !total +. (exp_dist_fun a b) /. tl in
  match data with
  | Single pr ->
      let tl = Gtree.tree_length (Placerun.get_ref_tree pr) 
      and pql = Placerun.get_pqueries pr in
      Base.list_iter_over_pairs_of_single (add_to_tot tl) pql;
      (* now do diagonal *)
      List.iter (fun pq -> add_to_tot tl pq pq) pql;
      let n = List.length pql in
      (!total) /. (float_of_int ((n*(n+1))/2))
  | Pairwise (pr, pr') ->
      let tl = Gtree.tree_length (Placerun.get_same_tree pr pr') in
      Base.list_iter_over_pairs_of_two 
        (add_to_tot tl)
        (Placerun.get_pqueries pr)
        (Placerun.get_pqueries pr');
      (!total) /. 
        (float_of_int ((Placerun.n_pqueries pr)*(Placerun.n_pqueries pr')))

let of_placerun_pair dist_fun exponent pr pr' = 
  of_placerun_gen dist_fun (Pairwise (pr,pr')) exponent

let of_placerun dist_fun exponent pr = 
  of_placerun_gen dist_fun (Single pr) exponent

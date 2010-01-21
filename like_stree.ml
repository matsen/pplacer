(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 * 
 * calculate the distal and proximal likelihood vectors at each site of the
 * reference tree.
 *)

open MapsSets
open Fam_batteries
open Stree

let like_aln_map_of_data seq_type align tree = 
  let like_aln = Alignment_funs.like_aln_of_align seq_type align in
  IntMap.map
    (Array.get like_aln)
    (Alignment_funs.makeAlnIndexMap 
      (Bark_map.to_name_map (Gtree.get_bark_map tree))
      (Array.map fst align))

let glv_arr_for model align tree = 
  Glv_arr.make
    ~n_glvs:(1 + Gtree.n_edges tree)
    ~n_sites:(Alignment.length align)
    ~n_rates:(Model.n_rates model)
    ~n_states:(Model.n_states model)

let calc_distal_and_evolv_dist model tree like_aln_map 
                          ~distal_glv_arr ~evolv_dist_glv_arr = 
  (* calc returns the evolv_dist for each subtree in a list *)
  let rec calc t = 
    let id = Stree.top_id t in
    let distal = Glv_arr.get distal_glv_arr id
    and evolv_dist = Glv_arr.get evolv_dist_glv_arr id
    in
  (* first calculate distal *)
    let () = match t with
    | Stree.Node(_, tL) -> begin
        (* take the product of the below *)
        Glv.listwise_product distal (List.map calc tL);
        Glv.perhaps_pull_exponent distal
      end
    | Stree.Leaf _ -> 
  (* for a leaf, distal is just the LV from the aln for each rate *)
        Glv.prep_constant_rate_glv_from_lv_arr
          distal
          (IntMap.find id like_aln_map)
    in
  (* now calculate evolv_dist *)
    Glv.evolve_into 
      model ~dst:evolv_dist ~src:distal (Gtree.get_bl tree id);
    evolv_dist
  in
  let _ = calc (Gtree.get_stree tree) in ()
 
(* pull_each_out : 
# pull_each_out [1;2;3];;
- : (int * int list) list = [(1, [2; 3]); (2, [1; 3]); (3, [1; 2])]
*)
let pull_each_out init_list = 
  let rec aux all_pairs start = function
    | x::l ->
        aux ((x, List.rev_append start l)::all_pairs) (x::start) l
    | [] -> all_pairs
  in
  List.rev (aux [] [] init_list)

(* pull out the glv corresponding to the id of the given tree *)
let glv_from_stree glv_arr t = Glv_arr.get glv_arr (Stree.top_id t)

let calc_proximal model tree 
                  evolved_prox ~evolv_dist_glv_arr ~proximal_glv_arr = 
(* calc calculates the proximal vectors. in contrast to the previous calc, the
 * recursion step is actually calculating the proximal vectors for the edges
 * below the given edge.
 *)
  let rec calc = function
    | Stree.Node(id, tL) -> 
        let proximal = Glv_arr.get proximal_glv_arr id in
        Glv.evolve_into 
          model ~dst:evolved_prox ~src:proximal (Gtree.get_bl tree id);
        List.iter
          (fun (chosen, rest) ->
            let prox_below = glv_from_stree proximal_glv_arr chosen in
            Glv.listwise_product 
              prox_below
              (evolved_prox::
                (List.map (glv_from_stree evolv_dist_glv_arr) rest));
            Glv.perhaps_pull_exponent prox_below)
          (pull_each_out tL);
        List.iter calc tL
    | Stree.Leaf _ -> ()
  in
  let stree = Gtree.get_stree tree in
  let top_prox = glv_from_stree proximal_glv_arr stree in
  Glv.set_exp_and_all_entries top_prox 0 1.;
  calc stree

(* this is our "main". the utils are just data structures of the same size as
 * the rest we can use for storing things *)
let calc_distal_and_proximal model tree like_aln_map util_glv
              ~distal_glv_arr ~proximal_glv_arr ~util_glv_arr = 
  calc_distal_and_evolv_dist model tree like_aln_map 
                    ~distal_glv_arr ~evolv_dist_glv_arr:util_glv_arr;
  calc_proximal model tree util_glv 
    ~evolv_dist_glv_arr:util_glv_arr ~proximal_glv_arr;
  ()


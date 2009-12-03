(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 * 
 * calculate the distal and proximal likelihood vectors at each site of the
 * reference tree.
 *)

open MapsSets
open Fam_batteries
open Stree


let qmat_map_of_bark_map diagdq bark_map rate = 
  IntMap.map 
    (fun bark ->
      try 
        diagdq#expWithT (bark#get_bl *. rate)
      with
      | Newick_bark.No_bl ->
          failwith "Branch length unspecified in qmat_map_of_bark_map")
    bark_map

let calc_distal_like_map_col nstates tree aln_index_map qmat_map col_like =
  let v = Gsl_vector.create nstates in
  let rec calc_col_like_list = function
    | Stree.Node(our_id, tL) -> 
        (* make calculations for daughter trees *)
        let below = List.map calc_col_like_list tL in
        (* our_like will become the like vector at our node *)
        let our_like = Gsl_vector.create ~init:1. nstates in
        List.iter (
          function
        (* top_like is the likelihood of the top node of the daughter tree *)
            | (top_id, top_like)::_ ->
                (* apply markov transformation to top_like *)
                Fam_gsl_matvec.matVecMul v (IntMap.find top_id qmat_map) top_like;
                for k=0 to nstates-1 do
                  our_like.{k} <- our_like.{k} *. v.{k}
                done;
            | [] -> assert(false);
        ) below;
        (our_id, our_like)::(List.flatten below)
    | Stree.Leaf(our_id) -> 
        [our_id, col_like.(IntMap.find our_id aln_index_map)]
  in
  IntMapFuns.of_pairlist (calc_col_like_list tree)

let calc_distal_like_map nstates tree aln_index_map qmat_map aln_like = 
  Array.map (calc_distal_like_map_col nstates tree aln_index_map qmat_map) aln_like

let calc_proximal_like_map_col nstates tree qmat_map col_distal_like_map =
  (* we are given the likelihood coming down from the rest of the tree *)
  let rec aux above_like = function
    | Stree.Node(our_id, tL) ->
        (* first we evolve the above like vector along our edge *)
        let transformed_above = 
          Fam_gsl_matvec.allocMatVecMul (IntMap.find our_id qmat_map) above_like in
        let tA = Array.of_list tL in
        let ntrees_below = Array.length tA in
        (* we similarly transform the distal likelihoods. these all contrib *)
        let transformed_distals = 
          Array.map (
            fun t -> 
              let top_id = Stree.top_id t in
              Fam_gsl_matvec.allocMatVecMul 
                (IntMap.find top_id qmat_map) 
                (IntMap.find top_id col_distal_like_map)
          ) tA
        in
        (* we will recur down each branch and collect the results in results *)
        let results = ref [] in
        (* chosen is the branch we choose to go down *)
        for chosen=0 to ntrees_below-1 do
          (* go through all of the trees below this node *)
          (* start with the like vector coming from above *)
          (* next_above will be the vector we pass down the tree. it starts with
           * transformed_above, which is the evolved vector coming from above *)
          let next_above = Gsl_vector.copy transformed_above in
          (* iter through each that is not the chosen one *)
          for not_chosen=0 to ntrees_below-1 do
            if chosen <> not_chosen then begin
              for state=0 to nstates-1 do
                  next_above.{state} <- 
                    next_above.{state} *.
                    transformed_distals.(not_chosen).{state}
              done    
            end
          done;
          (* collect the results of passing this like vector down the tree *)
          results := (aux next_above tA.(chosen))::(!results)
        done;
        (* add this node to the growing list. above_like is the proximal vector
         * for this edge. *)
        (our_id, above_like)::(List.flatten !results)
    | Stree.Leaf(our_id) -> [(our_id, above_like)]
  in
  try 
    IntMapFuns.of_pairlist (aux (Gsl_vector.create ~init:1. nstates) tree)
  with
  | Not_found -> failwith "Missing branch length in reference tree."

let calc_proximal_like_map nstates tree qmat_map distal_like_map = 
  Array.map 
    (calc_proximal_like_map_col nstates tree qmat_map) distal_like_map

let calc_distoproximal nstates aln_index_map aln_like diagdq gtree rate = 
  let stree = Gtree.get_stree gtree in
  let qmat_map = 
    qmat_map_of_bark_map diagdq (Gtree.get_bark_map gtree) rate in
  let distal = 
    calc_distal_like_map nstates stree aln_index_map qmat_map aln_like in
  let proximal = calc_proximal_like_map nstates stree qmat_map distal in
  (distal, proximal)

let distoproximal_of_aln_and_gtree seq_type diagdq align gtree rate = 
  let aln_index_map = 
    Alignment_funs.makeAlnIndexMap 
      (Bark_map.to_name_map (Gtree.get_bark_map gtree))
      (Array.map fst align) 
  in
  let seqs = Array.map snd align 
  and nstates = Alignment.nstates_of_seq_type seq_type in
  let aln_like = Alignment_funs.aln_like_of_unnamed_align seq_type seqs in
  calc_distoproximal nstates aln_index_map aln_like diagdq gtree rate

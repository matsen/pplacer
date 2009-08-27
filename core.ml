(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
*)

open Fam_batteries
open MapsSets
open Stree
open Placement

let max_iter = 100


(* pplacer_core :
  * actually try the placements, etc. return placement records *)
let pplacer_core verb_level tolerance write_masked
                 start_pend max_pend ratio_cutoff 
                 model ref_align istree query_align = 
  if verb_level >= 1 then begin
    print_endline "Running likelihood calculation on reference tree...";
    flush_all ()
  end;
  let seq_type = Model.seq_type model in
  (* prepare rgmas *)
  let all_locs = IntMapFuns.keys istree.info.bl in
  assert(all_locs <> []);
  (* warning: only good if locations are as above. may want to pass a smaller
   * selection of locations later *)
  let locs = ListFuns.remove_last all_locs in
  let (dmap, pmap) = 
    GlvIntMap.dp_of_data model seq_type ref_align istree locs in

  (* cycle through queries *)
  let num_queries = Array.length query_align in
  let ref_length = Alignment.length ref_align in
  (* the main query loop *)
  Array.mapi (
    fun query_num (query_name, query_seq) ->
      Base.print_time_fun "garbage collection" Gc.compact;
      if String.length query_seq <> ref_length then
        failwith ("query '"^query_name^"' is not the same length as the ref alignment");
      if verb_level >= 1 then begin
        Printf.printf "running '%s' %d / %d ...\n" query_name (query_num+1) num_queries; 
        flush_all ()
      end;
      (* prepare the query glv *)
      let query_arr = StringFuns.to_char_array query_seq in
      let mask_arr = 
        Array.map (fun c -> c <> '?' && c <> '-') query_arr in
      let query_like = 
        match seq_type with
        | Alignment.Nucleotide_seq -> Array.map NucModels.likeArrOfNuc query_arr
        | Alignment.Protein_seq -> Array.map ProtModels.likeArrOfAA query_arr
      in
      let query_glv = 
        Glv.lv_list_to_constant_rate_glv 
          (Model.n_rates model) 
          (Base.mask_to_list mask_arr query_like)
      in
     (* now we can mask the rgma, and the glv map we will use in three_tax *) 
      let d_masked_map = GlvIntMap.mask mask_arr dmap 
      and p_masked_map = GlvIntMap.mask mask_arr pmap in

      (* make a masked alignment with just the given query sequence and the
       * reference seqs *)
      if write_masked then
        Alignment.toFasta
          (Alignment.mask_align mask_arr
            (Alignment.stack [|query_name, query_seq|] ref_align))
          (query_name^".mask.fasta");

      (* first get the results from ML *)
      let ml_results = 
        List.map
          (fun loc ->
            (loc,
              let cut_bl = IntMap.find loc istree.info.bl in
              let make_initial glv_map start_bl = 
                Glv_edge.make 
                  model (IntMap.find loc glv_map) start_bl in
              (* make our three taxon tree *)
              let tt = 
                Three_tax.make 
                  model
                  ~dist:(make_initial d_masked_map (cut_bl /. 2.))
                  ~prox:(make_initial p_masked_map (cut_bl /. 2.))
                  ~query:(Glv_edge.make model query_glv start_pend)
                  ~cut_bl
              in
              (* optimize *)
              Three_tax.optimize tolerance max_pend max_iter tt;
              (* get the results *)
              Three_tax.get_results tt))
          locs 
      in
      (* calc ml weight ratios. these tuples are ugly but that way we don't need
       * to make a special type for ml results. *)
      let ratios = 
        Base.normalized_prob (
          List.map (fun (loc, (best_like, best_pend_bl, best_dist_bl)) -> best_like) 
            ml_results) in
      (* filter results by cutoff *)
      let ml_filtered_results = 
        List.sort (
          fun (loc, ml_ratio1, _, _, _) (loc, ml_ratio2, _, _, _) -> 
            - compare ml_ratio1 ml_ratio2) ( (* sort ml ratio in decreasing *)
          List.filter (
            fun (_, ml_ratio, _, _, _) -> ml_ratio > ratio_cutoff) (
              List.map2 (
                fun ml_ratio (loc, (best_like, best_pend_bl, best_dist_bl)) -> 
                  (loc, ml_ratio, best_like, best_pend_bl, best_dist_bl))
                ratios ml_results))
      in
      (query_name,
          List.map (
            fun (loc, ml_rat, best_like, best_pend_bl, best_dist_bl) ->
              {location = loc;
              ml_ratio = ml_rat;
              distal_bl = best_dist_bl;
              pendant_bl = best_pend_bl;
              log_like = best_like;
              marginal_prob = None;
              post_prob = None}
          ) ml_filtered_results
        )) query_align

            

  (*
let jc, statD = NucModels.diagd_and_statd_of_phyml_file "JC.stats.txt";;
let it = Stree.ofNewick "((x:0.2,y:3e-2):0.05,z:1e-5):0.";;
let mini = Alignment.readAlign "mini.fasta";;
let p = ThreeTax.pplacer_core 2 1e-2 0.1 1. Alignment.Nucleotide_seq jc statD mini it [|"hi", "ACAAA"|];;

let smallRefAlign = Alignment.readAlign "test/smallRefAlign.phy"
let query_align = Alignment.readAlign "test/fragment.fasta"
let small_it = Stree.ofNewickFile "test/small.tre"

let q = pplacer_core 2 1e-2 0.1 2. Alignment.Protein_seq ProtModels.lgDiagd 
                     ProtModels.lgFreq smallRefAlign small_it query_align
*)

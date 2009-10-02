(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * here we actually do the work.
 *
 * notes:
 * the h-stuff (hmap, etc.) is a way to cut down the number of locations that
 * we actually fully consider. we cut each edge in half, then evolve the distal
 * and proximal along those cut edges, then take the pairwise product of the
 * resulting likelihood vectors with the stationary distribution. call this
 * the h vector. that way, we can just come along with our query (which has
 * been evolved along start_pend) and take a dot with the h-vector to get the
 * likelihood of attaching the query sequence in the middle of the edge with a
 * pendant branch length of start_pend for the cost of a linear number of
 * dot products.
 *
*)

open Fam_batteries
open MapsSets
open Stree
open Prefs

let max_iter = 100

type prior = Uniform_prior | Exponential_prior of float

(* pplacer_core :
  * actually try the placements, etc. return placement records *)
let pplacer_core 
      prefs prior model ref_align istree 
      query_align ~dmap ~pmap locs = 
  let seq_type = Model.seq_type model in
  let half_evolve_glv_map loc g = 
    Glv.evolve model g ((IntMap.find loc istree.info.bl) /. 2.) in
  let halfd = IntMap.mapi half_evolve_glv_map dmap
  and halfp = IntMap.mapi half_evolve_glv_map pmap
  in
  let prior_fun =
    match prior with
    | Uniform_prior -> (fun _ -> 1.)
    | Exponential_prior mean ->
        fun pend -> Gsl_randist.exponential_pdf ~mu:mean pend
  in
  (* cycle through queries *)
  let num_queries = Array.length query_align in
  let ref_length = Alignment.length ref_align in
  (* the main query loop *)
  Array.mapi
    (fun query_num (query_name, query_seq) ->
      (* Base.print_time_fun "garbage collection" Gc.compact; *)
      if String.length query_seq <> ref_length then
        failwith ("query '"^query_name^"' is not the same length as the ref alignment");
      if (verb_level prefs) >= 1 then begin
        Printf.printf "running '%s' %d / %d ...\n" query_name (query_num+1) num_queries; 
        flush_all ()
      end;
      (* prepare the query glv *)
      let query_arr = StringFuns.to_char_array query_seq in
      let mask_arr = 
        Array.map (fun c -> c <> '?' && c <> '-') query_arr in
      let query_like = 
        match seq_type with
        | Alignment.Nucleotide_seq -> Array.map Nuc_models.likeArrOfNuc query_arr
        | Alignment.Protein_seq -> Array.map ProtModels.likeArrOfAA query_arr
      in
      let query_glv = 
        Glv.lv_list_to_constant_rate_glv 
          (Model.n_rates model) 
          (Base.mask_to_list mask_arr query_like)
      in
      (* make a masked alignment with just the given query sequence and the
       * reference seqs *)
      if (write_masked prefs) then
        Alignment.toFasta
          (Alignment.mask_align mask_arr
            (Alignment.stack [|query_name, query_seq|] ref_align))
          (query_name^".mask.fasta");
     (* now we can mask the rgma, and the glv map we will use in three_tax *) 
      let curr_time = Sys.time () in
      let d_masked_map = GlvIntMap.mask mask_arr dmap 
      and p_masked_map = GlvIntMap.mask mask_arr pmap 
      and half_d_maskd = GlvIntMap.mask mask_arr halfd 
      and half_p_maskd = GlvIntMap.mask mask_arr halfp 
      in
      if (verb_level prefs) >= 2 then Printf.printf "masking took\t%g\n" ((Sys.time ()) -. curr_time);
      (* make our edges.
       * start them all with query as a place holder.
       * we are breaking interface by naming them and changing them later, but
       * it would be silly to have setting functions for each edge.  *)
      let dist_edge = Glv_edge.make model query_glv (start_pend prefs)
      and prox_edge = Glv_edge.make model query_glv (start_pend prefs)
      and query_edge = Glv_edge.make model query_glv (start_pend prefs)
      in

      (* get the results from the h_map *)
      let q_evolved = Glv_edge.get_evolv query_edge in
      (* the h_ranking ranks the locations according to the h criterion. we use
       * this as an ordering for the slower computation *)
      let curr_time = Sys.time () in
      let h_r = 
        List.sort
          (fun (_,l1) (_,l2) -> - compare l1 l2)
          (List.map
            (fun loc ->
              (loc,
              Glv.log_like3_statd model 
                q_evolved 
                (IntMap.find loc half_d_maskd) 
                (IntMap.find loc half_p_maskd)))
            locs)
      in
      (* List.iter (fun (loc, like) -> Printf.printf "%d\t%g\n" loc like) h_r; print_endline ""; *)
      if (verb_level prefs) >= 2 then Printf.printf "ranking took\t%g\n" ((Sys.time ()) -. curr_time);
      let h_ranking = List.map fst h_r in
      (* first get the results from ML *)
      let curr_time = Sys.time () in
      (* make our three taxon tree. we can't move this higher because the cached
       * calculation on the edges needs to be the right size for the length of
       * the query sequence. *)
      let tt = 
        Three_tax.make 
          model
          ~dist:dist_edge ~prox:prox_edge ~query:query_edge
      in
      (* prepare_tt: set tt up for loc. side effect! *)
      let prepare_tt loc = 
        let cut_bl = IntMap.find loc istree.info.bl in
        (* this is just to factor out setting up the prox and dist edges
         * and setting their branch lengths to half the cut branch length *)
        let set_edge edge glv_map = 
          Glv_edge.set_orig_and_bl 
            model
            edge
            (IntMap.find loc glv_map) 
            (cut_bl /. 2.)
        in
        (* set the query edge to the default *)
        Glv_edge.set_bl model query_edge (start_pend prefs);
        (* set up the distal and proximal edges *)
        set_edge dist_edge d_masked_map;
        set_edge prox_edge p_masked_map;
        (* Printf.printf "tt: %d\t%g\n" loc (Three_tax.log_like tt); *)
      in
      let ml_evaluate_location loc = 
        prepare_tt loc;
              (* optimize *)
        let _ = 
          try
            Three_tax.optimize (tolerance prefs) (max_pend prefs) max_iter tt
          with
          | Minimization.ExceededMaxIter ->
              Printf.printf 
                "optimization for %s at %d exceeded maximum number of iterations.\n"
                query_name
                loc;
        in
        (* get the results *)
        Three_tax.get_results tt
      in
      (* in play_ball we go down the h_ranking list and wait until we get
       * strike_limit strikes, i.e. placements that are strike_box below the
       * best one so far. *)
      let rec play_ball like_record n_strikes results = function
        | loc::rest -> begin
            try 
              let (best_like,_,_) as result = 
                ml_evaluate_location loc in
              let new_results = (loc, result)::results in
              if List.length results >= (max_pitches prefs) then
                new_results
              else if best_like > like_record then
                (* we have a new best likelihood *)
                play_ball best_like n_strikes new_results rest
              else if best_like < like_record-.(strike_box prefs) then
                (* we have a strike *)
                if n_strikes+1 >= (max_strikes prefs) then new_results
                else play_ball like_record (n_strikes+1) new_results rest
              else
                (* not a strike, just keep on accumulating results *)
                play_ball like_record n_strikes new_results rest
            with
              | Gsl_error.Gsl_exn(_,_) ->
                  Printf.printf "Warning: GSL had a problem with location %d for query %s; it was skipped.\n" loc query_name;
                  play_ball like_record n_strikes results rest
           end
        | [] -> results
      in
      let ml_results = 
        if max_strikes prefs = 0 then
          (* we have disabled ball playing, and evaluate every location *)
          List.map (fun loc -> (loc, ml_evaluate_location loc)) locs
        else
          play_ball (-. infinity) 0 [] h_ranking 
      in
      if (verb_level prefs) >= 2 then Printf.printf "ML calc took\t%g\n" ((Sys.time ()) -. curr_time);
      (* calc ml weight ratios. these tuples are ugly but that way we don't need
       * to make a special type for ml results. *)
      let ml_ratios = 
        Base.ll_normalized_prob 
          (List.map 
            (fun (_, (best_like, _, _)) -> best_like) 
            ml_results) 
      in
      let ml_sorted_results = 
        Placement.sort_placecoll Placement.ml_ratio 
          (List.map2 
            (fun ml_ratio (loc, (log_like, pend_bl, dist_bl)) -> 
              Placement.make_ml 
                loc ~ml_ratio ~log_like ~pend_bl ~dist_bl)
            ml_ratios ml_results)
      in
      if (calc_pp prefs) then begin
        let curr_time = Sys.time () in
        (* pp calculation *)
        (* calculate marginal likes *)
        let marginal_probs = 
          List.map 
            (fun placement ->
              prepare_tt (Placement.location placement);
              Three_tax.calc_marg_prob 
                prior_fun (pp_rel_err prefs) (max_pend prefs) tt)
            ml_sorted_results
        in
        if (verb_level prefs) >= 2 then Printf.printf "PP calc took\t%g\n" ((Sys.time ()) -. curr_time);
        (query_name, 
          Placement.filter_placecoll 
            Placement.ml_ratio 
            (ratio_cutoff prefs) 
            (ListFuns.map3 
              (fun placement marginal_prob post_prob ->
                Placement.add_pp placement ~marginal_prob ~post_prob)
              ml_sorted_results
              marginal_probs
              (Base.ll_normalized_prob marginal_probs)))
      end
      else
        (query_name, 
        Placement.filter_placecoll 
          Placement.ml_ratio (ratio_cutoff prefs) ml_sorted_results)
    ) query_align

  (*
let jc, statD = diagd_and_statd_of_phyml_file "JC.stats.txt";;
let it = Stree.ofNewick "((x:0.2,y:3e-2):0.05,z:1e-5):0.";;
let mini = Alignment.readAlign "mini.fasta";;
let p = ThreeTax.pplacer_core 2 1e-2 0.1 1. Alignment.Nucleotide_seq jc statD mini it [|"hi", "ACAAA"|];;

let smallRefAlign = Alignment.readAlign "test/smallRefAlign.phy"
let query_align = Alignment.readAlign "test/fragment.fasta"
let small_it = Stree.ofNewickFile "test/small.tre"

let q = pplacer_core 2 1e-2 0.1 2. Alignment.Protein_seq ProtModels.lgDiagd 
                     ProtModels.lgFreq smallRefAlign small_it query_align
*)

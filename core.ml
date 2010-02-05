(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * here we actually do the work.
*)

open Fam_batteries
open MapsSets
open Prefs

let max_iter = 200
let prof_prefix_req = 1
(* the most number of placements we keep *)
let keep_at_most = 5
(* we throw away anything that has ml_ratio below keep_factor * (best ml_ratio) *)
let keep_factor = 0.05

type prior = Uniform_prior | Exponential_prior of float

(* pplacer_core :
  * actually try the placements, etc. return placement records *)
let pplacer_core 
      mem_usage prefs query_fname prior model ref_align gtree 
      ~darr ~parr ~halfd ~halfp locs = 
  let seq_type = Model.seq_type model
  and update_usage () = 
    let c = Memory.curr_gb () in
    if c > !mem_usage then mem_usage := c
  and prior_fun =
    match prior with
    | Uniform_prior -> (fun _ -> 1.)
    | Exponential_prior mean ->
        fun pend -> Gsl_randist.exponential_pdf ~mu:mean pend
  and query_channel = Fasta_channel.of_fname query_fname 
  and ref_length = Alignment.length ref_align
  and prof_trie = ref Ltrie.empty
  and fantasy_mat = 
    if fantasy prefs <> 0. then
      Fantasy.make_fantasy_matrix 
        ~max_strike_box:(int_of_float (strike_box prefs))
        ~max_strikes:(max_strikes prefs)
    else [||]
  and fantasy_mod = Base.round (100. *. (fantasy_frac prefs))
  and n_fantasies = ref 0
  and split_keep_and_not ml_sorted_results = 
    assert(ml_sorted_results <> []);
    let best_ratio = Placement.ml_ratio (List.hd ml_sorted_results) in
    ListFuns.partitioni 
      (fun i p -> 
        ((i < keep_at_most) &&
        (Placement.ml_ratio p >= keep_factor *. best_ratio)))
      ml_sorted_results
  in
  (* set up the number of pitches and strikes according to the prefs *)
  let (t_max_pitches, t_max_strikes) = 
    if fantasy prefs <> 0. then
  (* in fantasy mode we evaluate the first max_pitches locations *)
      (max_pitches prefs, max_int)
    else if max_strikes prefs = 0 then 
  (* we have disabled ball playing, and evaluate every location *)
      (max_int, max_int)
    else
  (* usual ball playing *)
      (max_pitches prefs, max_strikes prefs)
  and num_queries = 
    Fasta_channel.size_checking_for_duplicate_names query_channel
  in
  let result_arr = 
    Array.make num_queries
    (Pquery.make Placement.ml_ratio ~name:"" ~seq:"" [])
  in
  (* *** the main query loop *** *)
  let process_query query_num (query_name, pre_query_seq) = 
    if fantasy prefs = 0. || query_num mod fantasy_mod = 0 then begin
    (* we only proceed if fantasy baseball is turned off or if this is one of
     * the sequences used for the fantasy baseball procedure *)
    let query_seq = String.uppercase pre_query_seq in
    update_usage ();
    if Memory.ceiling_compaction (max_memory prefs) then 
      if (verb_level prefs) >= 1 then begin
        print_endline "performed garbage compaction.";
        Memory.check_ceiling (max_memory prefs);
      end;
    if String.length query_seq <> ref_length then
      failwith ("query '"^query_name^"' is not the same length as the ref alignment");
    if (verb_level prefs) >= 1 then begin
      Printf.printf "running '%s' %d / %d ...\n" query_name (query_num+1) num_queries; 
      flush_all ()
    end;
    (* prepare the query glv *)
    let query_arr = StringFuns.to_char_array query_seq in
    (* the mask array shows true if it's included *)
    let mask_arr = 
      Array.map (fun c -> c <> '?' && c <> '-') query_arr in
    let query_like = 
      match seq_type with
      | Alignment.Nucleotide_seq -> Array.map Nuc_models.lv_of_nuc query_arr
      | Alignment.Protein_seq -> Array.map Prot_models.lv_of_aa query_arr
    in
    let query_glv = 
      Glv.lv_list_to_constant_rate_glv 
        (Model.n_rates model) 
        (Base.mask_to_list mask_arr query_like)
    in
    (* make a masked alignment with just the given query sequence and the
     * reference seqs *)
    if write_masked prefs then
      Alignment.toFasta
        (Alignment.mask_align mask_arr
          (Alignment.stack [|query_name, query_seq|] ref_align))
        (query_name^".mask.fasta");
   (* mask *) 
    let curr_time = Sys.time () in
    let darr_masked = Glv_arr.mask mask_arr darr 
    and parr_masked = Glv_arr.mask mask_arr parr 
    and halfd_maskd = Glv_arr.mask mask_arr halfd 
    and halfp_maskd = Glv_arr.mask mask_arr halfp 
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
    (* the h_r ranks the locations according to the h criterion. we use
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
              (Glv_arr.get halfd_maskd loc) 
              (Glv_arr.get halfp_maskd loc)))
          locs)
    in
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
    (* finding a friend *)
    let friend =
      if (profile_len prefs) = 0 then None (* profiling turned off *)
      else begin
        let prof = Ltrie.list_first_n h_ranking (profile_len prefs) in
        let friend_result = 
          if Ltrie.prefix_mem prof prof_prefix_req !prof_trie then 
            (* we pass the prefix test *)
            begin
              let f_index = Ltrie.int_approx_find prof !prof_trie in
              if (verb_level prefs) >= 2 then
                Printf.printf "found friend in %d\n" f_index;
              assert(f_index < query_num);
              Some (result_arr.(f_index))
            end
          else None
        in
        prof_trie := Ltrie.add prof query_num !prof_trie;
        friend_result
      end
    in
    let get_friend_place loc = 
      match friend with
      | None -> None
      | Some f -> Pquery.opt_place_by_location f loc
    in 
(* set tt edges to be for location loc with given pendant and distal branch lengths *)
    let set_tt_edges loc ~pendant ~distal = 
      let cut_bl = Gtree.get_bl gtree loc in
      let set_edge edge glv_arr len = 
        Glv_edge.set_orig_and_bl 
          model
          edge
          (Glv_arr.get glv_arr loc) 
          len
      in
      (* still the same glv for query, but have to set branch length *)
      Glv_edge.set_bl model query_edge pendant;
      (* need to set the glv and branch length for dist and prox *)
      set_edge dist_edge darr_masked distal;
      set_edge prox_edge parr_masked (cut_bl -. distal)
    in
    let tt_edges_from_placement p = 
      let loc = Placement.location p in
      set_tt_edges loc 
        ~pendant:(Placement.pendant_bl p)
        ~distal:(Placement.distal_bl p)
    in
    (* prepare_tt: set tt up for loc. side effect! *)
    let prepare_tt loc = 
    (* set up branch lengths, using a friend's branch lengths if we have them *)
      match get_friend_place loc with
      | None -> (* set to usual defaults *)
          let cut_bl = Gtree.get_bl gtree loc in
          set_tt_edges loc 
            ~pendant:(start_pend prefs)
            ~distal:(cut_bl /. 2.);
      | Some f -> (* use a friend's branch lengths *)
          tt_edges_from_placement f
    in
    let ml_evaluate_location loc = 
      prepare_tt loc;
      (* optimize *)
      let () = 
        try
          let n_like_calls = 
            Three_tax.optimize (tolerance prefs) (max_pend prefs) max_iter tt
          in
          if 2 < verb_level prefs then
            Printf.printf "\tlocation %d: %d likelihood function calls\n" loc n_like_calls;
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
    let gsl_warn loc query_name warn_str = 
      Printf.printf "Warning: GSL problem with location %d for query %s; Skipped with warning \"%s\".\n" 
                    loc query_name warn_str;
    in
    (* in play_ball we go down the h_ranking list and wait until we get
     * strike_limit strikes, i.e. placements that are strike_box below the
     * best one so far. *)
    let rec play_ball like_record n_strikes results = function
      | loc::rest -> begin
          try 
            let (like,_,_) as result = 
              ml_evaluate_location loc in
            let new_results = (loc, result)::results in
            if List.length results >= t_max_pitches then
              new_results
            else if like > like_record then
              (* we have a new best likelihood *)
              play_ball like n_strikes new_results rest
            else if like < like_record-.(strike_box prefs) then
              (* we have a strike *)
              if n_strikes+1 >= t_max_strikes then new_results
              else play_ball like_record (n_strikes+1) new_results rest
            else
              (* not a strike, just keep on accumulating results *)
              play_ball like_record n_strikes new_results rest
          with
(* we need to handle the exception here so that we continue the baseball recursion *)
          | Gsl_error.Gsl_exn(_,warn_str) ->
              gsl_warn loc query_name warn_str;
              play_ball like_record n_strikes results rest
         end
      | [] -> results
    in
    let ml_results = 
 (* important to reverse for fantasy baseball. also should save time on sorting *)
      List.rev (play_ball (-. infinity) 0 [] h_ranking) 
    in
    if (verb_level prefs) >= 2 then Printf.printf "ML calc took\t%g\n" ((Sys.time ()) -. curr_time);
    if fantasy prefs <> 0. then begin
      Fantasy.add_to_fantasy_matrix ml_results fantasy_mat;
      incr n_fantasies;
    end;
    (* calc ml weight ratios. these tuples are ugly but that way we don't need
     * to make a special type for ml results. *)
    let ml_ratios = 
      Base.ll_normalized_prob 
        (List.map 
          (fun (_, (like, _, _)) -> like) 
          ml_results) 
    in
    let ml_sorted_results = 
      Placement.sort_placecoll 
        Placement.ml_ratio
        (List.map2 
          (fun ml_ratio (loc, (log_like, pend_bl, dist_bl)) -> 
            Placement.make_ml 
              loc ~ml_ratio ~log_like ~pend_bl ~dist_bl)
          ml_ratios ml_results)
    in
    let keep, not_keep = split_keep_and_not ml_sorted_results in
  (* the tricky thing here is that we want to retain the optimized branch
   * lengths for all of the pqueries that we try so that we can use them later
   * as friends. however, we don't want to calculate pp for all of them, and we
   * don't want to return them for writing either *)
    result_arr.(query_num) <-
      Pquery.make_ml_sorted
        ~name:query_name 
        ~seq:query_seq
        (if (calc_pp prefs) then begin
          (* pp calculation *)
          let curr_time = Sys.time () in
        (* calculate marginal likes for those placements we will keep *)
          let marginal_probs = 
            List.map 
              (fun placement ->
                tt_edges_from_placement placement;
                Three_tax.calc_marg_prob 
                  prior_fun (pp_rel_err prefs) (max_pend prefs) tt)
              keep
          in
          (* just add pp to those we will keep *)
          if (verb_level prefs) >= 2 then Printf.printf "PP calc took\t%g\n" ((Sys.time ()) -. curr_time);
            ((ListFuns.map3 
              (fun placement marginal_prob post_prob ->
                Placement.add_pp placement ~marginal_prob ~post_prob)
              keep
              marginal_probs
              (Base.ll_normalized_prob marginal_probs))
          (* retain the ones we will throw away, but don't calc pp *)
              @ not_keep)
        end
        else ml_sorted_results)
  end
  in
  Fasta_channel.named_seq_iteri process_query query_channel;
  if fantasy prefs <> 0. then begin
    Fantasy.results_to_file 
      (Filename.basename (Filename.chop_extension query_fname))
      fantasy_mat (!n_fantasies);
      Fantasy.print_optimum fantasy_mat (fantasy prefs) (!n_fantasies);
  end;
  (* here we filter things away we don't write them to file *)
  let results = 
    Array.map 
      (Pquery.apply_to_place_list 
        (fun pl -> fst(split_keep_and_not pl))) result_arr
  in
  update_usage ();
  results


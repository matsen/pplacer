(* Here we actually do the work.
 * The optimization is split into two stages-- the first "rough" stage and the
 * second stage where branches and locations are more thoroughly optimized.
 * See http://www.biomedcentral.com/1471-2105/11/538/#sec5
*)

open Ppatteries
open Prefs

let max_iter = 200

(* The second stage tolerance for branch length optimization. Modify in online
 * help if changed here. *)
let final_tolerance = 1e-5

let avg l = (List.reduce (+.) l) /. (List.length l |> float_of_int)

(* Make an array that indexes the true values of the array.
# Core.mask_reindex [|true; false; false; true; false; true;|];;
- : int array = [|0; 3; 5|]
* When applied to a mask indicator array, it gives an array such that the ith
* index of the reindexed array is the index of the original sequence
* corresponding to the ith index of the masked sequence.
*)
let mask_reindex a =
  Array.fold_lefti (fun l i elt -> if elt then i::l else l) [] a
  |> List.rev
  |> Array.of_list

(* This function returns a map from the internal nodes i to the mean of the
 * prior used for the pendant branch lengths. This is the average of the leaf
 * distances strictly below the edge corresponding to the internal node i, plus
 * top_segment_f applied to the edge labeled i. *)
let prior_mean_map top_segment_f t =
  let bl = Gtree.get_bl t in
  let rec aux = function
    | Stree.Leaf i -> IntMap.singleton i (top_segment_f (bl i)), [bl i]
    | Stree.Node (i, subtrees) ->
      let map, distances = List.map aux subtrees
        |> List.reduce
            (fun (m1, l1) (m2, l2) -> IntMap.union m1 m2, List.append l1 l2)
      in
      (* Add the average distance for i onto the map. *)
      distances
        |> List.map ((+.) (top_segment_f (bl i)))
        |> avg
        |> flip (IntMap.add i) map,
      (* Increase the collection of distances by the given branch length. *)
      List.map ((+.) (bl i)) distances
  in
  aux t.Gtree.stree |> fst |> IntMap.remove (Gtree.top_id t)

type prior =
  | Uniform_prior
  | Flat_exp_prior of float
  | Informative_exp_prior of float IntMap.t

type result =
  | Fantasy of (int * (float * float * float)) list
  | Pquery of Pquery.pquery
  | Timing of string * float
  | Evaluated_best of string * int * int * int

  (* Prefs.prefs -> *)
  (* Ppatteries.IntMap.key list -> *)
  (* prior -> *)
  (* Model.t -> *)
  (* (string * string) Ppatteries.Array.mappable -> *)
  (* < get_bl : float; .. > Gtree.gtree -> *)
  (* darr:Glv.glv array -> *)
  (* parr:Glv.glv array -> *)
  (* snodes:Glv.glv array -> string * string -> result list *)

(* pplacer_core :
 * Actually try the placements, etc. Return placement records. *)
let pplacer_core (type a) (type b) m prefs figs prior (model: a) ref_align gtree ~(darr: b array) ~(parr: b array) ~(snodes: b array) =
  let module Model = (val m: Glvm.Model with type t = a and type glv_t = b) in
  let module Glv = Model.Glv in
  let module Glv_arr = Glv_arr.Make(Model) in
  let module Glv_edge = Glv_edge.Make(Model) in
  let module Three_tax = Three_tax.Make(Model) in
  let keep_at_most = Prefs.keep_at_most prefs
  and keep_factor = Prefs.keep_factor prefs in
  let log_keep_factor = log keep_factor in
  let seq_type = Model.seq_type model
  (* The prior function takes an edge number and a pendant BL. *)
  and prior_fun =
    match prior with
      | Uniform_prior -> (fun _ _ -> 1.)
      | Flat_exp_prior mean -> fun _ -> Gsl.Randist.exponential_pdf ~mu:mean
      | Informative_exp_prior mean_map ->
        fun id -> Gsl.Randist.exponential_pdf ~mu:(IntMap.find id mean_map)
  and ref_length = Alignment.length ref_align in
  let utilv_nsites = Gsl.Vector.create ref_length in
  (* Set up the number of pitches and strikes according to the prefs. *)
  let (t_max_pitches, t_max_strikes) =
    if fantasy prefs <> 0. then
      (* In fantasy mode we evaluate the first max_pitches locations. *)
      (max_pitches prefs, max_int)
    else if max_strikes prefs = 0 then
      (* We have disabled ball playing, and evaluate every location. *)
      (max_int, max_int)
    else
      (* Usual ball playing. *)
      (max_pitches prefs, max_strikes prefs)
  in
  (* Making glvs which are appropriate for query side of the first placement
   * stage. In contrast to the second stage query glv, this guy is full length. *)
  let full_query_orig = Model.make_glv model ~n_sites:ref_length
  in
  let full_query_evolv = Glv.mimic full_query_orig in
  (* *** The main query loop. *** *)
  let process_query ?show_query (query_name, query_seq) =
    begin match show_query with
      | Some fn -> fn query_name
      | None -> Printf.printf ">%s\n" query_name; flush_all ()
    end;
    if String.length query_seq <> ref_length then
      failwith ("query '"^query_name^"' is not the same length as the ref alignment");
    (* Turn the query sequence into a character array. *)
    let query_arr = StringFuns.to_char_array query_seq in
    let lv_arr_of_char_arr a =
      match seq_type with
        | Alignment.Nucleotide_seq -> Array.map Nuc_models.lv_of_nuc a
        | Alignment.Protein_seq -> Array.map Prot_models.lv_of_aa a
    in
    (* *** First prepare for the first phase of optimization. ***
     * This phase is not masked. Make full_query_orig, the glv for the unmasked
     * likelihood vector, which will be used for the first stage optimization
     * only. Note that it only has one rate. *)
    Glv.prep_constant_rate_glv_from_lv_arr
      full_query_orig
      (lv_arr_of_char_arr query_arr);
    (* Making full_query_evolv, which is full_query_orig after it has been
     * evolved along the fixed pendant branch length used for the first stage
     * of optimization. *)
    Model.evolve_into
      model
      ~dst:full_query_evolv
      ~src:full_query_orig
      (start_pend prefs);
    (* *** Now begin preparing for the three-taxon tree! ***
     * This tree will be masked on both the query and the reference side of
     * things. First make an array showing which sites are informative in that
     * query sequence. *)
    let mask_arr = Array.map Alignment.informative query_arr in
    (* The corresponding vector version. *)
    let mask_vec =
      Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout
        (Array.map (fun b -> if b then 1 else 0) mask_arr)
    in
    (* reind_arr has the reindexing induced by the masking (see mask_reindex
     * above). *)
    let reind_arr = mask_reindex mask_arr in
    (* Subset sites of query_arr to just informative sequences. *)
    let masked_query_arr = Array.filter Alignment.informative query_arr in
    if masked_query_arr = [||] then
      failwith ("sequence '"^query_name^"' has no informative sites.");
    (* Write out a masked alignment with just the given query sequence and the
     * reference sequences if desired. *)
    if write_masked prefs then
      Alignment.to_fasta
        (Alignment.mask_align mask_arr
           (Alignment.stack [|query_name, query_seq|] ref_align))
        ((out_dir prefs)^"/"^query_name^".mask.fasta");
    let query_glv =
      Model.make_constant_rate_glv_from_lv_arr
        model
        (lv_arr_of_char_arr masked_query_arr)
    in
    (* Make the edges for our three-taxon tree that will be used for the second
     * stage of optimization. We will breaking interface by changing
     * them in place later, but it would be silly to have setting functions for
     * each edge. *)
    (* Note that Gmix_models are homogenous across sites, so we don't need to pass
       a reind_arr. Gcat_models aren't. *)
    let edge_make_fn a b c = match Model.get_model_class () with
      | Glvm.Gmix_model -> Glv_edge.make a b c
      | Glvm.Gcat_model -> Glv_edge.make ~reind_arr a b c
    in
    let dist_edge = edge_make_fn model (Glv.mimic query_glv) (start_pend prefs)
    and prox_edge = edge_make_fn model (Glv.mimic query_glv) (start_pend prefs)
    and pend_edge = edge_make_fn model query_glv (start_pend prefs)
    in
    let curr_time = Sys.time () in
    let logdots = ref 0 in
    let score loc =
      incr logdots;
      Glv.masked_logdot
        utilv_nsites
        full_query_evolv
        (Glv_arr.get snodes loc)
        mask_vec
    in
    (* The h_ranking ranks the locations according to the first stage of
     * optimization. We use this as an ordering for the second stage. *)
    let h_ranking = Fig.enum_by_score score (strike_box prefs) figs
    and best_seen = ref None in
    let h_ranking =
      if evaluate_all prefs then
        (* Keep track of what the best location we've seen is if we're going to
         * compare this against the actual best location. *)
        Enum.map
          (tap
             (fun (score, loc) -> match !best_seen with
               | Some (prev_score, _) when prev_score >= score -> ()
               | _ -> best_seen := Some (score, loc)))
          h_ranking
      else
        h_ranking
    in
    let results = [Timing ("ranking", (Sys.time ()) -. curr_time)] in
    (* first get the results from ML *)
    let curr_time = Sys.time () in
    (* make our three taxon tree. we can't move this higher because the cached
     * calculation on the edges needs to be the right size for the length of
     * the query sequence. *)
    let tt =
      Three_tax.make
        model
        utilv_nsites
        ~dist:dist_edge ~prox:prox_edge ~pend:pend_edge
    in
    (* set tt edges to be for location loc with given pendant and distal branch lengths *)
    let set_tt_edges loc ~pendant ~distal =
      let cut_bl = Gtree.get_bl gtree loc in
      let set_edge edge glv_arr len =
        Glv.mask_into
          mask_arr
          ~src:(Glv_arr.get glv_arr loc)
          ~dst:(Glv_edge.get_orig edge);
        Glv_edge.set_bl model edge len
      in
      (* still the same glv for query, but have to set branch length *)
      Glv_edge.set_bl model pend_edge pendant;
      (* need to set the glv and branch length for dist and prox *)
      set_edge dist_edge darr distal;
      set_edge prox_edge parr (cut_bl -. distal)
    in
    let tt_edges_default loc =
      let cut_bl = Gtree.get_bl gtree loc in
      set_tt_edges loc
        ~pendant:(start_pend prefs)
        ~distal:(cut_bl /. 2.)
    in
    let tt_edges_from_placement p =
      let loc = Placement.location p in
      set_tt_edges loc
        ~pendant:(Placement.pendant_bl p)
        ~distal:(Placement.distal_bl p)
    in
    (* prepare_tt: set tt up for loc. side effect! *)
    let prepare_tt loc = tt_edges_default loc in
    (* evaluate the location wrt ML *)
    let ml_optimize_location mlo_tolerance loc =
      let () =
        try
          let n_like_calls =
            Three_tax.optimize mlo_tolerance (max_pend prefs) max_iter tt
          in
          dprintf ~l:2 "\tlocation %d: %d likelihood function calls\n" loc n_like_calls;
        with
          | Minimization.ExceededMaxIter ->
            dprintf
              "optimization for %s at %d exceeded maximum number of iterations.\n"
              query_name
              loc;
      in
      (* get the results *)
      Three_tax.get_results tt
    in
    let safe_ml_optimize_location tol loc =
      try ml_optimize_location tol loc with
        | Gsl.Error.Gsl_exn(_,_) -> begin
          (* try again starting with default branch lengths *)
          tt_edges_default loc;
          ml_optimize_location tol loc
        end
    in
    (* in play_ball we go down the h_ranking list and wait until we get
     * strike_limit strikes, i.e. placements that are strike_box below the
     * best one so far. *)
    let rec play_ball like_record n_strikes results = match Enum.get h_ranking with
      | Some (_, loc) ->
        prepare_tt loc;
        begin match begin
          try
            Some (safe_ml_optimize_location (initial_tolerance prefs) loc)
          with Gsl.Error.Gsl_exn(_,warn_str) ->
            dprintf
              "Warning: GSL problem with location %d for query %s; Skipped with warning \"%s\".\n"
              loc
              query_name
              warn_str;
            None
        end with
          | None -> play_ball like_record n_strikes results
          | Some ((like,_,_) as result) -> (* ... *)
        let new_results = (loc, result)::results in
        if List.length results >= t_max_pitches then
          new_results
        else if like > like_record then
          (* we have a new best likelihood *)
          play_ball like n_strikes new_results
        else if like < like_record-.(strike_box prefs) then
          (* we have a strike *)
          if n_strikes+1 >= t_max_strikes then new_results
          else play_ball like_record (n_strikes+1) new_results
        else
          (* not a strike, just keep on accumulating results *)
          play_ball like_record n_strikes new_results
        end
      | None -> results
    in
    let ml_results =
      (* important to reverse for fantasy baseball. also should save time on sorting *)
      List.rev (play_ball (-. infinity) 0 [])
    in
    if ml_results = [] then
      failwith
        (Printf.sprintf "empty results for %s!\n" query_name);
    let results = Timing ("ML calculation", (Sys.time ()) -. curr_time) :: results
      |> maybe_cons
          (if evaluate_all prefs then
             let logdots = !logdots in
             let best_loc_complete = Fig.enum_all figs |> Enum.arg_max score
             and _, best_loc_seen = Option.get !best_seen in
             Some (Evaluated_best (query_name, logdots, best_loc_complete, best_loc_seen))
           else None)
    in
    if fantasy prefs <> 0. then
      Fantasy ml_results :: results
    else begin
      (* these tuples are ugly but that way we don't need
       * to make a special type for ml results. *)
      let get_like (_, (like, _, _)) = like in
      let sorted_ml_results =
        List.sort (comparing get_like |> flip) ml_results in
      assert(sorted_ml_results <> []);
      let best_like = get_like (List.hd sorted_ml_results) in
      let _ = match classify_float best_like with
        | FP_infinite | FP_nan ->
          dprintf "Warning: encountered %g for final likelihood.\n" best_like;
        | _ -> ()
      in
      let keep_results, _ =
        ListFuns.partitioni
          (fun i r ->
            ((i < keep_at_most) &&
                (get_like r >= log_keep_factor +. best_like)))
          sorted_ml_results
      in
    (* do final refinement of branch lengths *)
      let refined_results =
        List.map
          (fun initial ->
            let (loc, (_, pendant, distal)) = initial in
            set_tt_edges loc ~pendant ~distal;
            try
              (loc, safe_ml_optimize_location final_tolerance loc)
            with
              | Gsl.Error.Gsl_exn(_,warn_str) ->
                dprintf
                  "Warning: GSL problem with final branch length optimization for location %d. %s\n"
                  loc
                  warn_str;
                initial)
          keep_results
      in
      let sorted_ml_placements =
        List.map2
          (fun ml_ratio (loc, (log_like, pend_bl, dist_bl)) ->
            Placement.make_ml
              loc ~ml_ratio ~log_like ~pend_bl ~dist_bl)
          (ll_normalized_prob (List.map get_like refined_results))
          refined_results
      in
      let results, placements =
        if calc_pp prefs then begin
          (* pp calculation *)
          let curr_time = Sys.time () in
          (* calculate marginal likes for those placements we will keep *)
          let marginal_probs =
            List.map
              (fun placement ->
                tt_edges_from_placement placement;
                Three_tax.calc_marg_prob
                  (prior_fun (Placement.location placement))
                  (pp_rel_err prefs)
                  (max_pend prefs)
                  tt)
              sorted_ml_placements
          in
          (* add pp *)
          Timing ("PP calculation", (Sys.time ()) -. curr_time) :: results,
          ((ListFuns.map3
              (fun placement marginal_prob post_prob ->
                Placement.add_pp placement ~marginal_prob ~post_prob)
              sorted_ml_placements
              marginal_probs
              (ll_normalized_prob marginal_probs)))
        end
        else results, sorted_ml_placements
      in
      Pquery (Pquery.make_ml_sorted
        ~namlom:[query_name, 1.]
        ~seq:query_seq
        placements) :: results
    end
  in
  process_query

(* Support for the gamma mixture model of sequence heterogeneity.
 * *)

open Ppatteries
open Gstar_support

module Model: Glvm.Model =
struct

  type t = {
    statd: Gsl_vector.vector;
    diagdq: Diagd.t;
    seq_type: Alignment.seq_type;
    rates: float array;
    tensor: Tensor.tensor; (* multi-rate transition matrix for the model *)
  }

  let statd model = model.statd
  let diagdq model = model.diagdq
  let rates model = model.rates
  let tensor model = model.tensor
  let seq_type model = model.seq_type
  let n_states model = Alignment.nstates_of_seq_type model.seq_type
  let n_rates model = Array.length (rates model)

  let build ref_align = function
    | Glvm.Gmix_model (model_name, empirical_freqs, transitions, rates) ->
      let seq_type, (trans, statd) =
        Gstar_support.seqtype_and_trans_statd_of_info
          model_name transitions empirical_freqs ref_align
      in
      let n_states = Alignment.nstates_of_seq_type seq_type in
      {
        statd; seq_type; rates;
        diagdq = Diagd.normed_of_exchangeable_pair trans statd;
        tensor = Tensor.create (Array.length rates) n_states n_states;
      }

    | _ -> invalid_arg "build"

  let write ch model =
    Format.fprintf
      (Format.formatter_of_out_channel ch)
      "@[%s model@\n@\nstat distn:@\n%a@\n@\nsite rates:@\n%a@]@."
      (Alignment.seq_type_to_str model.seq_type)
      Linear_utils.ppr_gsl_vector model.statd
      Ppr.ppr_float_array model.rates

  (* prepare the tensor for a certain branch length *)
  let prep_tensor_for_bl model bl =
    Diagd.multi_exp model.tensor model.diagdq model.rates bl

  module Glv =
  struct

    type t = {
      e: (int, BA.int_elt, BA.c_layout) BA1.t;
      a: Tensor.tensor;
    }

    let get_n_rates g = Tensor.dim1 g.a
    let get_n_sites g =
      let n = Tensor.dim2 g.a in
      assert(n = BA1.dim g.e);
      n
    let get_n_states g = Tensor.dim3 g.a

    let dims g = get_n_rates g, get_n_sites g, get_n_states g

    let ppr ff g =
      Format.fprintf ff "@[{ e = %a; @,a = %a }@]"
        iba1_ppr g.e
        Tensor.ppr g.a

    let make ~n_rates ~n_sites ~n_states = {
      e = iba1_create n_sites;
      a = Tensor.create n_rates n_sites n_states;
    }

    (* make a glv of the same dimensions *)
    let mimic x = {
      e = iba1_mimic x.e;
      a = Tensor.mimic x.a;
    }

    (* deep copy *)
    let copy x = {
      e = iba1_copy x.e;
      a = Tensor.copy x.a;
    }

    let memcpy ~dst ~src =
      BA1.blit src.e dst.e;
      BA3.blit src.a dst.a

    let set_unit g =
      BA1.fill g.e 0;
      BA3.fill g.a 1.

    (* Find the "worst" fpclass of the floats in g. *)
    let fp_classify g =
      Tensor.fp_classify g.a

    (* set g according to function fe for exponenent and fa for entries *)
    let seti g fe fa =
      let n_sites = get_n_sites g
      and n_rates = get_n_rates g
      and n_states = get_n_states g in
      for site=0 to n_sites-1 do
        for rate=0 to n_rates-1 do
          for state=0 to n_states-1 do
            g.a.{rate,site,state} <- fa ~rate ~site ~state
          done
        done;
        g.e.{site} <- fe site
      done

    (* copy the site information from src to dst. _i is which site to copy. *)
    let copy_site ~src_i ~src ~dst_i ~dst =
      (dst.e).{dst_i} <- (src.e).{src_i};
      for rate=0 to (get_n_rates src)-1 do
        BA1.blit (BA3.slice_left_1 src.a rate src_i)
          (BA3.slice_left_1 dst.a rate dst_i)
      done

    (* copy the sites marked with true in site_mask_arr from src to dst. the number
     * of trues in site_mask_arr should be equal to the number of sites in dst. *)
    let mask_into site_mask_arr ~src ~dst =
      let dst_n_sites = get_n_sites dst in
      let dst_i = ref 0 in
      Array.iteri
        (fun src_i b ->
          if b then begin
            assert(!dst_i < dst_n_sites);
            copy_site ~src ~src_i ~dst_i:(!dst_i) ~dst;
            incr dst_i;
          end)
        site_mask_arr;
      assert(!dst_i = dst_n_sites)

    (* This is used when we have a pre-allocated GLV and want to fill it with a
     * same-length lv array. Zero pulled exponents as well. "Constant rate"
     * to the fact that the glvs at the internal nodes have different lvs for
     * each rate. For this one we have them all set to the same lvs, which
     * makes sense when, say, we are at a leaf.
     *)
    let prep_constant_rate_glv_from_lv_arr g lv_arr =
      assert(lv_arr <> [||]);
      assert(get_n_sites g = Array.length lv_arr);
      assert(get_n_states g = Gsl_vector.length lv_arr.(0));
      seti g
        (fun _ -> 0)
        (fun ~rate:_ ~site ~state ->
          lv_arr.(site).{state})

    (* *** Pulling exponents. *** *)

    (* pull out the exponent if it's below min_allowed_twoexp and return it. this
     * process is a bit complicated by the fact that we are partitioned by rate, as
     * can be seen below. *)
    let perhaps_pull_exponent min_allowed_twoexp g =
      let n_rates = get_n_rates g
      and n_sites = get_n_sites g in
      let max_twoexp = ref (-max_int) in
      (* cycle through sites *)
      for site=0 to n_sites-1 do
        max_twoexp := (-max_int);
        (* first find the max twoexp *)
        for rate=0 to n_rates-1 do
          let s = BA3.slice_left_1 g.a rate site in
          let (_, twoexp) = frexp (Gsl_vector.max s) in
          if twoexp > !max_twoexp then max_twoexp := twoexp
        done;
        (* now scale if it's needed *)
        if !max_twoexp < min_allowed_twoexp then begin
          for rate=0 to n_rates-1 do
            (* take the negative so that we "divide" by 2^our_twoexp *)
            Gsl_vector.scale
              (BA3.slice_left_1 g.a rate site)
              (of_twoexp (-(!max_twoexp)));
          done;
          (* bring the exponent out *)
          g.e.{site} <- g.e.{site} + !max_twoexp;
        end
      done

    (* *** likelihood calculations *** *)

    (* the log "dot" of the likelihood vectors in the 0-indexed interval
     * [start,last] *)
    let bounded_logdot utilv_nsites x y start last =
      assert(dims x = dims y);
      assert(start >= 0 && start <= last && last < get_n_sites x);
      (Linear.ten_bounded_logdot
         x.a y.a start last utilv_nsites)
      +. (log_of_2 *. ((bounded_total_twoexp x.e start last) +.
                          (bounded_total_twoexp y.e start last)))

    (* just take the log "dot" of the likelihood vectors *)
    let logdot utilv_nsites x y =
      bounded_logdot utilv_nsites x y 0 ((get_n_sites x)-1)

    (* take the pairwise product of glvs g1 and g2, then store in dest. *)
    let pairwise_prod ~dst g1 g2 =
      assert(dims g1 = dims g2);
      iba1_pairwise_sum dst.e g1.e g2.e;
      Linear.ten_pairwise_prod dst.a g1.a g2.a

    (* take the product of all of the GLV's in the list, then store in dst.
     * could probably be implemented more quickly, but typically we are only
     * taking pairwise products anyway. we pull out the x::y below to optimize
     * for that case. *)
    let listwise_prod dst = function
      | x::y::rest ->
        (* first product of first two *)
        pairwise_prod ~dst x y;
        (* now take product with each of the rest *)
        List.iter (pairwise_prod ~dst dst) rest
      | [src] ->
        (* just copy over *)
        memcpy ~dst ~src
      | [] -> assert(false)


    (* For verification purposes. *)

    let get_a g ~rate ~site ~state = BA3.get g.a rate site state

    (* pick the ML state by taking the sum across rates for each state and site *)
    let summarize_post summarize_f initial g =
      let n_sites = get_n_sites g
      and n_states = get_n_states g
      and n_rates = get_n_rates g in
      let summary = Array.make n_sites initial
      and u = Gsl_vector.create ~init:0. n_states in
      for site=0 to n_sites-1 do
        Gsl_vector.set_all u 0.;
        for rate=0 to n_rates-1 do
          for state=0 to n_states-1 do
            u.{state} <- u.{state} +. (get_a ~rate ~site ~state g)
          done
        done;
        summary.(site) <- summarize_f u
      done;
      summary

  end

  type glv_t = Glv.t

  let make_glv model =
    Glv.make
      ~n_states:(n_states model)
      ~n_rates:(n_rates model)

  let mmap_glv_arrays model fd shared n_arrays n_glvs ~n_sites =
    gen_mmap_glv_arrays
      fd
      shared
      n_arrays
      n_glvs
      [|n_sites|]
      [|n_rates model; n_sites; n_states model|]
      (fun ega aga ->
        {Glv.e = BA.array1_of_genarray ega; Glv.a = BA.array3_of_genarray aga})

  let size_of_glv_arrays model n_arrays n_glvs ~n_sites =
    gen_size_of_glv_arrays
      (n_states model * n_rates model)
      n_sites
      n_arrays
      n_glvs

  (* Make a glv out of a list of likelihood vectors. For comments, see
   * prep_constant_rate_glv_from_lv_arr. *)
  let make_constant_rate_glv_from_lv_arr model lv_arr =
    assert(lv_arr <> [||]);
    let g = Glv.make
      ~n_rates:(n_rates model)
      ~n_sites:(Array.length lv_arr)
      ~n_states:(Gsl_vector.length lv_arr.(0)) in
    Glv.prep_constant_rate_glv_from_lv_arr g lv_arr;
    g

  (* take the log like of the product of three things then dot with the stationary
   * distribution. *)
  let log_like3 model utilv_nsites x y z =
    assert(Glv.dims x = Glv.dims y && Glv.dims y = Glv.dims z);
    (Linear.ten_log_like3 (statd model) x.Glv.a y.Glv.a z.Glv.a utilv_nsites)
    +. (log_of_2 *.
          ((total_twoexp x.Glv.e)
          +. (total_twoexp y.Glv.e)
          +. (total_twoexp z.Glv.e)))

  (* evolve_into: evolve src according to model for branch length bl, then
   * store the results in dst. *)
  let evolve_into model ~dst ~src bl =
    (* copy over the exponents *)
    BA1.blit src.Glv.e dst.Glv.e;
    (* prepare the matrices in our matrix cache *)
    prep_tensor_for_bl model bl;
    (* The matrices for various rates are juxtaposed in the tensor. *)
    tensor_mul (tensor model) ~dst:dst.Glv.a ~src:src.Glv.a

  (* take the pairwise product of glvs g1 and g2, incorporating the
   * stationary distribution, then store in dest. *)
  let statd_pairwise_prod model ~dst g1 g2 =
    assert(Glv.dims g1 = Glv.dims g2);
    iba1_pairwise_sum dst.Glv.e g1.Glv.e g2.Glv.e;
    Linear.ten_statd_pairwise_prod (statd model) dst.Glv.a g1.Glv.a g2.Glv.a

  (* Make the per-site log likelihood array. *)
  let site_log_like_arr3 model x y z =
    let log_site_likes = Array.make (Glv.get_n_sites x) 0.
    and f_n_rates = float_of_int (n_rates model)
    and statd = statd model
    in
    for site=0 to (Glv.get_n_sites x)-1 do
      let site_like = ref 0. in
      for rate=0 to (Glv.get_n_rates x)-1 do
        for state=0 to (Glv.get_n_states x)-1 do
          site_like := !site_like +.
            statd.{state}
          *. (Glv.get_a x ~rate ~site ~state)
          *. (Glv.get_a y ~rate ~site ~state)
          *. (Glv.get_a z ~rate ~site ~state)
        done;
      done;
      if 0. >= !site_like then
        failwith (Printf.sprintf "Site %d has zero likelihood." site);
      log_site_likes.(site) <-
        log(!site_like /. f_n_rates)
        +. log_of_2 *.
          (float_of_int (x.Glv.e.{site} + y.Glv.e.{site} + z.Glv.e.{site}))
    done;
    log_site_likes

  let slow_log_like3 model x y z =
    Array.fold_left ( +. ) 0. (site_log_like_arr3 model x y z)

  (* We don't do anything for model refinement. *)
  let refine _ _ _ _ _ _ = ()
  let check _ _ = ()
  let mask_sites _ _ = ()

end

module Glv_edge = Glv_edge.Make(Model)
module Glv_arr = Glv_arr.Make(Model)

let init_of_prefs ref_dir_complete prefs ref_align =
  let opt_transitions = match Prefs.stats_fname prefs with
    | s when s = "" ->
      Printf.printf
        "NOTE: you have not specified a stats file. I'm using the %s model.\n"
        (Prefs.model_name prefs);
      None
    | _ -> Parse_stats.parse_stats ref_dir_complete prefs
  in
  if Alignment.is_nuc_align ref_align && (Prefs.model_name prefs) <> "GTR" then
    dprint "WARNING: You have given me what appears to be a nucleotide alignment, but have specified a model other than GTR. I only know GTR for nucleotides!\n";
  Glvm.Gmix_model
    ((Prefs.model_name prefs),
     (Prefs.empirical_freqs prefs),
     opt_transitions,
     (Gamma.discrete_gamma
        (Prefs.gamma_n_cat prefs) (Prefs.gamma_alpha prefs)))

(* deprecated now *)
let init_of_stats_fname prefs stats_fname ref_align =
  prefs.Prefs.stats_fname := stats_fname;
  init_of_prefs "" prefs ref_align

let init_of_json o ref_align =
  let model_name = safe_hashtbl_find o "subs_model" |> Jsontype.string in
  if Alignment.is_nuc_align ref_align && model_name <> "GTR" then
    dprint "WARNING: You have given me what appears to be a nucleotide alignment, but have specified a model other than GTR. I only know GTR for nucleotides!\n";
  if safe_hashtbl_find o "ras_model" |> Jsontype.string <> "gamma" then
    failwith "Whoops! This is supposed to be a gamma mixture model.";
  let gamma_o = safe_hashtbl_find o "gamma" in
  let opt_transitions =
    if Hashtbl.mem o "subs_rates" then begin
      let subs_rates_o = safe_hashtbl_find o "subs_rates" in
      Some [|
        Simple_json.find_float subs_rates_o "ac";
        Simple_json.find_float subs_rates_o "ag";
        Simple_json.find_float subs_rates_o "at";
        Simple_json.find_float subs_rates_o "cg";
        Simple_json.find_float subs_rates_o "ct";
        Simple_json.find_float subs_rates_o "gt";
           |]
    end
    else None
  in
  Glvm.Gmix_model
    (model_name,
     (safe_hashtbl_find o "empirical_frequencies" |> Jsontype.bool),
     opt_transitions,
     (Gamma.discrete_gamma
        (Simple_json.find_int gamma_o "n_cats")
        (Simple_json.find_float gamma_o "alpha")))

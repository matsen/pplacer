open Ppatteries
open Gstar_support

module Model: Glvm.Model =
struct

  (* this simply contains the information about the Markov process corresponding
   * to the model.
   *
   * we also include matrices mats which can be used as scratch to avoid having to
   * allocate for it. see prep_mats_for_bl below. *)

  type t = {
    statd: Gsl_vector.vector;
    diagdq: Diagd.t;
    seq_type: Alignment.seq_type;
    rates: float array;
    (* tensor is a tensor of the right shape to be a multi-rate transition matrix for the model *)
    tensor: Tensor.tensor;
    site_categories: int array;
    (* occupied_rates: bool array; *)
  }
  type model_t = t

  let statd model = model.statd
  let diagdq model = model.diagdq
  let rates model = model.rates
  let tensor model = model.tensor
  let seq_type model = model.seq_type
  let n_states model = Alignment.nstates_of_seq_type model.seq_type
  let n_rates model = Array.length (rates model)

  let build ref_align = function
    | Glvm.Gcat_model (model_name, emperical_freqs, transitions, rates, site_categories) ->
      let seq_type, (trans, statd) =
        if model_name = "GTR" then
          (Alignment.Nucleotide_seq,
           match transitions with
             | Some transitions ->
               (Nuc_models.b_of_trans_vector transitions,
                Alignment.emper_freq 4 Nuc_models.nuc_map ref_align)
             | None -> failwith "GTR specified but no substitution rates given.")
        else
          (Alignment.Protein_seq,
           let model_trans, model_statd =
             Prot_models.trans_and_statd_of_model_name model_name in
           (model_trans,
            if emperical_freqs then
              Alignment.emper_freq 20 Prot_models.prot_map ref_align
            else
              model_statd))
      in
      let n_states = Alignment.nstates_of_seq_type seq_type in
      {
        statd; seq_type; rates; site_categories;
        diagdq = Diagd.normed_of_exchangeable_pair trans statd;
        tensor = Tensor.create (Array.length rates) n_states n_states;
      }

    | _ -> invalid_arg "build"

  (* prepare the tensor for a certain branch length *)
  let prep_tensor_for_bl model bl =
    Diagd.multi_exp model.tensor model.diagdq model.rates bl

  module Glv =
  struct

    (* glvs *)
    type t = {
      e: (int, BA.int_elt, BA.c_layout) BA1.t;
      a: Matrix.matrix;
    }

    let get_n_sites g =
      let n = Matrix.dim1 g.a in
      assert(n = BA1.dim g.e);
      n

    let get_n_states g = Matrix.dim2 g.a

    let dims g = get_n_sites g, get_n_states g

    let ppr ff g =
      Format.fprintf ff "@[{ e = %a; @,a = %a }@]"
        iba1_ppr g.e
        Matrix.ppr g.a

    let make ~n_sites ~n_states = {
      e = iba1_create n_sites;
      a = Matrix.create n_sites n_states;
    }

    (* make a glv of the same dimensions *)
    let mimic x = {
      e = iba1_mimic x.e;
      a = Matrix.mimic x.a;
    }

    (* deep copy *)
    let copy x = {
      e = iba1_copy x.e;
      a = Matrix.copy x.a;
    }

    let memcpy ~dst ~src =
      BA1.blit src.e dst.e;
      Matrix.blit src.a dst.a

    let set_unit g =
      BA1.fill g.e 0;
      Matrix.fill g.a 1.

    (* Find the "worst" fpclass of the floats in g. *)
    let fp_classify g =
      Matrix.fp_classify g.a

    (* set g according to function fe for exponenent and fa for entries *)
    let seti g fe fa =
      let n_sites = get_n_sites g
      and n_states = get_n_states g in
      for site=0 to n_sites-1 do
        for state=0 to n_states-1 do
          g.a.{site, state} <- fa ~site ~state
        done;
        g.e.{site} <- fe site
      done

    (* copy the site information from src to dst. _i is which site to copy. *)
    let copy_site ~src_i ~src ~dst_i ~dst =
      (dst.e).{dst_i} <- (src.e).{src_i};
      BA1.blit (Matrix.slice_left src.a src_i)
        (Matrix.slice_left dst.a dst_i)

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

    (* this is used when we have a pre-allocated GLV and want to fill it with a
     * same-length lv array. zero pulled exponents as well. *)
    let prep_constant_rate_glv_from_lv_arr g lv_arr =
      assert(lv_arr <> [||]);
      assert(get_n_sites g = Array.length lv_arr);
      assert(get_n_states g = Gsl_vector.length lv_arr.(0));
      seti g
        (fun _ -> 0)
        (fun ~site ~state -> lv_arr.(site).{state})

    (* *** pulling exponent *** *)

    (* pull out the exponent if it's below min_allowed_twoexp and return it. this
     * process is a bit complicated by the fact that we are partitioned by rate, as
     * can be seen below. *)
    let perhaps_pull_exponent min_allowed_twoexp g =
      let n_sites = get_n_sites g in
      let max_twoexp = ref (-max_int) in
      (* cycle through sites *)
      for site=0 to n_sites-1 do
        let _, twoexp = Matrix.slice_left g.a site
          |> Gsl_vector.max
          |> frexp
        in
        (* now scale if it's needed *)
        if twoexp < min_allowed_twoexp then begin
          (* take the negative so that we "divide" by 2^our_twoexp *)
          Gsl_vector.scale
            (Matrix.slice_left g.a site)
            (of_twoexp (-twoexp));
          (* bring the exponent out *)
          g.e.{site} <- g.e.{site} + twoexp;
        end
      done

    (* *** likelihood calculations *** *)

    (* the log "dot" of the likelihood vectors in the 0-indexed interval
     * [start,last] *)
    let bounded_logdot utilv_nsites x y start last =
      assert(dims x = dims y);
      assert(start >= 0 && start <= last && last < get_n_sites x);
      (Linear.mat_bounded_logdot
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
      Linear.mat_pairwise_prod dst.a g1.a g2.a

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

    let get_a g ~site ~state = Matrix.get g.a site state

    (* pick the ML state by taking the sum across rates for each state and site *)
    let summarize_post summarize_f initial g =
      let n_sites = get_n_sites g
      and n_states = get_n_states g in
      let summary = Array.make n_sites initial
      and u = Gsl_vector.create ~init:0. n_states in
      for site=0 to n_sites-1 do
        Gsl_vector.set_all u 0.;
        for state=0 to n_states-1 do
          u.{state} <- u.{state} +. (get_a ~site ~state g)
        done;
        summary.(site) <- summarize_f u
      done;
      summary

  end

  type glv_t = Glv.t

  let make_glv model =
    Glv.make
      ~n_states:(n_states model)

  (* this is used when we want to make a glv out of a list of likelihood
   * vectors. differs from below because we want to make a new one. used to be
   * called `lv_arr_to_constant_rate_glv`. *)
  let lv_arr_to_glv model lv_arr =
    assert(lv_arr <> [||]);
    let g = Glv.make
      ~n_sites:(Array.length lv_arr)
      ~n_states:(Gsl_vector.length lv_arr.(0)) in
    Glv.prep_constant_rate_glv_from_lv_arr g lv_arr;
    g


  (* take the log like of the product of three things then dot with the stationary
   * distribution. *)
  let log_like3 model utilv_nsites x y z =
    assert(Glv.dims x = Glv.dims y && Glv.dims y = Glv.dims z);
    (Linear.mat_log_like3 (statd model)
       x.Glv.a
       y.Glv.a
       z.Glv.a
       utilv_nsites)
    +. (log_of_2 *.
          ((total_twoexp x.Glv.e) +. (total_twoexp y.Glv.e) +. (total_twoexp z.Glv.e)))


  (* evolve_into: evolve src according to model for branch length bl, then
   * store the results in dst. *)
  let evolve_into model ~dst ~src bl =
    (* copy over the exponents *)
    BA1.blit src.Glv.e dst.Glv.e;
    (* prepare the matrices in our matrix cache *)
    prep_tensor_for_bl model bl;
    (* apply transform specified by model on the a component *)
    let mat_by_cat cat = BA3.slice_left_2 model.tensor cat in
    for i=0 to (Glv.get_n_sites src) - 1 do
      let src_mat = BA2.slice_left src.Glv.a i
      and dst_mat = BA2.slice_left dst.Glv.a i
      and evo_mat = mat_by_cat model.site_categories.(i)
      in
      Linear_utils.mat_vec_mul dst_mat evo_mat src_mat
    done


  (* take the pairwise product of glvs g1 and g2, incorporating the
   * stationary distribution, then store in dest. *)
  let statd_pairwise_prod model ~dst g1 g2 =
    assert(Glv.dims g1 = Glv.dims g2);
    iba1_pairwise_sum dst.Glv.e g1.Glv.e g2.Glv.e;
    Linear.mat_statd_pairwise_prod (statd model) dst.Glv.a g1.Glv.a g2.Glv.a

  let slow_log_like3 model x y z =
    let ll_tot = ref 0.
    and statd = statd model
    in
    for site=0 to (Glv.get_n_sites x)-1 do
      let site_like = ref 0. in
      for state=0 to (Glv.get_n_states x)-1 do
        site_like := !site_like +.
          statd.{state}
        *. (Glv.get_a x ~site ~state)
        *. (Glv.get_a y ~site ~state)
        *. (Glv.get_a z ~site ~state)
      done;
      if 0. >= !site_like then
        failwith (Printf.sprintf "Site %d has zero likelihood." site);
      ll_tot := !ll_tot
      +. log(!site_like)
      +. log_of_2 *.
        (float_of_int (x.Glv.e.{site} + y.Glv.e.{site} + z.Glv.e.{site}))
    done;
    !ll_tot

end

module Glv_edge = Glv_edge.Make(Model)
module Glv_arr = Glv_arr.Make(Model)

let init_of_json o ref_align =
  let model_name = Hashtbl.find o "subs_model" |> Jsontype.string in
  if Alignment.is_nuc_align ref_align && model_name <> "GTR" then
    failwith "You have given me what appears to be a nucleotide alignment, but have specified a model other than GTR. I only know GTR for nucleotides!";
  let empirical_freqs = Hashtbl.find o "empirical_frequencies" |> Jsontype.bool
  and opt_transitions = Hashtbl.Exceptionless.find o "subs_rates"
    |> Option.map
        (Jsontype.obj
         |- (fun tbl ->
           List.map
             (Hashtbl.find tbl |- Jsontype.float)
             ["ac"; "ag"; "at"; "cg"; "ct"; "gt"])
         |- Array.of_list)
  and price_cat = Hashtbl.find o "Price-CAT" |> Jsontype.obj in
  let rates = Hashtbl.find price_cat "Rates"
    |> Jsontype.array
    |> List.map Jsontype.float
    |> Array.of_list
  and site_categories = Hashtbl.find price_cat "SiteCategories"
    |> Jsontype.array
    |> List.map Jsontype.int
    |> Array.of_list
  in
  Glvm.Gcat_model
    (model_name, empirical_freqs, opt_transitions, rates, site_categories)

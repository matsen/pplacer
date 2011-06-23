(* this simply contains the information about the Markov process corresponding
 * to the model.
 *
 * we also include matrices mats which can be used as scratch to avoid having to
 * allocate for it. see prep_mats_for_bl below.
 * *)

open Fam_batteries

type t =
  {
   statd     :  Gsl_vector.vector;
   diagdq    :  Diagd.t;
   seq_type  :  Alignment.seq_type;
   rates     :  float array;
   (* tensor is a tensor of the right shape to be a multi-rate transition matrix for the model *)
   tensor    :  Tensor.tensor;
  }

let statd    model = model.statd
let diagdq   model = model.diagdq
let rates    model = model.rates
let tensor   model = model.tensor
let seq_type model = model.seq_type
let n_states model = Alignment.nstates_of_seq_type model.seq_type
let n_rates  model = Array.length (rates model)

let build model_name emperical_freqs opt_transitions ref_align rates =
  let seq_type, (trans, statd) =
    if model_name = "GTR" then
      (Alignment.Nucleotide_seq,
      match opt_transitions with
      | Some transitions ->
          (Nuc_models.b_of_trans_vector transitions,
          Alignment_funs.emper_freq 4 Nuc_models.nuc_map ref_align)
      | None -> failwith "GTR specified but no substitution rates given.")
    else
      (Alignment.Protein_seq,
        let model_trans, model_statd =
          Prot_models.trans_and_statd_of_model_name model_name in
        (model_trans,
          if emperical_freqs then
            Alignment_funs.emper_freq 20 Prot_models.prot_map ref_align
          else
            model_statd))
  in
  let n_states = Alignment.nstates_of_seq_type seq_type in
  {
    statd = statd;
    diagdq = Diagd.normed_of_exchangeable_pair trans statd;
    seq_type = seq_type;
    rates = rates;
    tensor = Tensor.create (Array.length rates) n_states n_states;
  }

let of_prefs ref_dir_complete prefs ref_align =
  let opt_transitions = match Prefs.stats_fname prefs with
  | s when s = "" ->
      Printf.printf
        "NOTE: you have not specified a stats file. I'm using the %s model.\n"
        (Prefs.model_name prefs);
      None
  | _ -> Parse_stats.parse_stats ref_dir_complete prefs
  in
  if Alignment_funs.is_nuc_align ref_align && (Prefs.model_name prefs) <> "GTR" then
    failwith "You have given me what appears to be a nucleotide alignment, but have specified a model other than GTR. I only know GTR for nucleotides!";
  build
    (Prefs.model_name prefs)
    (Prefs.emperical_freqs prefs)
    opt_transitions
    ref_align
    (Gamma.discrete_gamma
      (Prefs.gamma_n_cat prefs) (Prefs.gamma_alpha prefs))

let of_json json_fname ref_align =
  let o = Simple_json.of_file json_fname in
  let model_name = (Simple_json.find_string o "subs_model") in
  if Alignment_funs.is_nuc_align ref_align && model_name <> "GTR" then
    failwith "You have given me what appears to be a nucleotide alignment, but have specified a model other than GTR. I only know GTR for nucleotides!";
  if "gamma" <> Simple_json.find_string o "ras_model" then
    failwith "For the time being, we only support gamma rates-across-sites model.";
  let gamma_o = Simple_json.find o "gamma" in
  let opt_transitions =
    if Simple_json.mem o "subs_rates" then begin
      let subs_rates_o = Simple_json.find o "subs_rates" in
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
  build
    model_name
    (Simple_json.find_bool o "empirical_frequencies")
    opt_transitions
    ref_align
    (Gamma.discrete_gamma
      (Simple_json.find_int gamma_o "n_cats")
      (Simple_json.find_float gamma_o "alpha"))

(* prepare the tensor for a certain branch length *)
let prep_tensor_for_bl model bl =
  Diagd.multi_exp model.tensor model.diagdq model.rates bl

let get_symbol code = function
  | -1 -> '-'
  | i -> try code.(i) with | Invalid_argument _ -> assert(false)

let to_sym_str code ind_arr =
  StringFuns.of_char_array (Array.map (get_symbol code) ind_arr)

let code model =
  match seq_type model with
    | Alignment.Nucleotide_seq -> Nuc_models.nuc_code
    | Alignment.Protein_seq -> Prot_models.prot_code


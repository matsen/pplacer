(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * this simply contains the information about the Markov process corresponding
 * to the model.
 *
 * we also include matrices mats which can be used as scratch to avoid having to
 * allocate for it. see prep_mats_for_bl below.
 * *)

open Fam_batteries

type t =
  { 
   statd     :  Gsl_vector.vector;
   diagdq    :  Diagd.diagd;
   seq_type  :  Alignment.seq_type;
   rates     :  float array;
   (* tensor is a tensor of the right shape to be a multi-rate transition matrix for the model *)
   tensor    :  Tensor.tensor;
   (* util_v is a float bigarray of the same length as the ref alignment *)
   util_v    :  Gsl_vector.vector;
  }

let statd    model = model.statd
let diagdq   model = model.diagdq
let rates    model = model.rates
let tensor   model = model.tensor 
let util_v   model = model.util_v 
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
      | None -> assert(false)) 
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
    diagdq = Diagd.normalizedOfExchangeableMat trans statd;
    seq_type = seq_type;
    rates = rates;
    tensor = Tensor.create (Array.length rates) n_states n_states;
    util_v = Gsl_vector.create (Alignment.length ref_align)
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

(* prepare the tensor for a certain branch length *)
let prep_tensor_for_bl model bl = 
  (model.diagdq)#multi_exp model.tensor model.rates bl

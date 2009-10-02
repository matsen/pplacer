(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

(* model.ml
 * this simply contains the information about the Markov process corresponding
 * to the model.
 * meant to be opened
 * *)

type model =
  { 
   statd     :  Gsl_vector.vector;
   diagdq    :  Diagd.diagd;
   seq_type  :  Alignment.seq_type;
   rates     :  float array;
  }

let statd    model = model.statd
let diagdq   model = model.diagdq
let rates    model = model.rates
let seq_type model = model.seq_type
let n_states model = Alignment.nstates_of_seq_type model.seq_type
let n_rates  model = Array.length (rates model)


let build model_name emperical_freqs opt_freqs_transitions ref_align rates =
  let seq_type, (trans, statd) = 
    if model_name = "GTR" then 
      (Alignment.Nucleotide_seq, 
      match opt_freqs_transitions with
      | Some(freqs, transitions) ->
          (Nuc_models.b_of_trans_vector transitions,
          Gsl_vector.of_array freqs)
      | None -> assert(false)) 
    else
      (Alignment.Protein_seq,
        let model_trans, model_statd = 
          ProtModels.trans_and_statd_of_model_name model_name in
        (model_trans,
          if emperical_freqs then
            AlignmentFuns.emper_freq 20 ProtModels.prot_map ref_align 
          else
            model_statd))
  in
  { statd = statd;
  diagdq = Diagd.normalizedOfExchangeableMat trans statd;
  seq_type = seq_type;
  rates = rates }


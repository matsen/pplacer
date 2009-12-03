(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * an aln_like is an arrray (sites) array (taxa). 
*)

open MapsSets
open Fam_batteries

let check_for_repeats name_arr = 
  let _ = 
    Array.fold_left (
      fun found_list name ->
        if List.mem name found_list then
          failwith("repeated taxon name: "^name)
        else
          name::found_list
    ) [] name_arr in
  ()

(* check to make sure that each site contains a nucleotide type symbol *)
let is_nuc_align aln = 
  try
    Array.iter
      (fun (_,seq) ->
        String.iter
          (fun nuc -> 
            let _ = CharMap.find nuc Nuc_models.nuc_map in ())
          seq)
    aln;
    true
  with
  | Not_found -> false


(* makeAlnIndexMap : make a map which maps from the node number to the correct column of the
 * alignment *)
let makeAlnIndexMap taxonMap alnNameArr = 
  let n_tree = IntMapFuns.nkeys taxonMap 
  and n_aln = Array.length alnNameArr in
  if n_tree <> n_aln then
    failwith 
      (Printf.sprintf "tree has %d taxa, and ref align has %d." n_tree n_aln);
  check_for_repeats alnNameArr;
  IntMap.map (
    fun taxName ->
      let outEdges = ArrayFuns.find_all_indices taxName alnNameArr in
      if List.length outEdges = 0 then 
        failwith ("taxon not found in alignment: '"^taxName^"'")
      else if List.length outEdges > 1 then 
        failwith ("taxon in alignment repeatedly: '"^taxName^"'")
      else (* pigeonhole principle *)
        List.hd outEdges
  ) taxonMap

let transposeAln uAlign = 
  if uAlign = [||] then invalid_arg "empty alignment!";
  let alnMat = Alignment.toMatrixUnnamed uAlign in
  Array.init (Array.length alnMat.(0)) (
    fun j -> 
      Array.init (Array.length alnMat) (
        fun i -> alnMat.(i).(j)))

let alignmentLikeOfUnnamedNucAlignment uAlign = 
  Array.map (Array.map Nuc_models.likeArrOfNuc) (transposeAln uAlign)

let alignmentLikeOfUnnamedProtAlignment uAlign = 
  Array.map (Array.map Prot_models.likeArrOfAA) (transposeAln uAlign)

let aln_like_of_unnamed_align seq_type seqs = 
  match seq_type with
  | Alignment.Nucleotide_seq -> 
      alignmentLikeOfUnnamedNucAlignment seqs
  | Alignment.Protein_seq -> 
      alignmentLikeOfUnnamedProtAlignment seqs

(* getting emperical frequencies from alignments 
 # AlignmentFuns.emper_freq 20 Prot_models.prot_map [|"","?ARRV"; "","AAVY?"|];;
- : Gsl_vector.vector =
{0.375; 0.25; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 
0.; 0.125; 0.25}
 *)
let emper_freq nstates like_map align = 
  let no_missing_normed = 
    CharMap.remove '-' (
    CharMap.remove '?' ( (* we don't remove 'X'... *)
    CharMap.map (
      fun like_vect -> 
        Fam_gsl_matvec.alloc_l1_normalize like_vect) like_map)) in
  let total = Gsl_vector.create ~init:0. nstates in
  Array.iter (
    fun (name, seq) ->
      String.iter (
        fun base -> 
          if base <> '-' && base <> '?' then
            if CharMap.mem base no_missing_normed then
              Gsl_vector.add total (CharMap.find base no_missing_normed)
            else
              failwith (Printf.sprintf "'%c' not a known base in %s!" base name)
      ) seq
  ) align;
  Fam_gsl_matvec.l1_normalize total;
  (* Format.fprintf Format.std_formatter "%a@." Fam_gsl_matvec.ppr_gsl_vector total; *)
  total


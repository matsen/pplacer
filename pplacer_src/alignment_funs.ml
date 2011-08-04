(* functions for dealing with alignments, especially for likelihoods
*)

open Batteries
open MapsSets
open Fam_batteries

let check_for_repeats name_arr =
  let _ =
    Array.fold_left
      (fun s name ->
        if StringSet.mem name s then
          failwith("repeated taxon name in alignment: "^name)
        else
          StringSet.add name s)
      StringSet.empty
      name_arr
  in
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

(* makeAlnIndexMap: make a map which maps from the node number to the row number of the
 * alignment. *)
let makeAlnIndexMap taxonMap alnNameArr =
  let n_tree = IntMap.cardinal taxonMap
  and n_aln = Array.length alnNameArr in
  if n_tree <> n_aln then
    failwith
      (Printf.sprintf "tree has %d taxa, and ref align has %d." n_tree n_aln);
  check_for_repeats alnNameArr;
  IntMap.map
    (fun taxName ->
      let outEdges = ArrayFuns.find_all_indices taxName alnNameArr in
      if List.length outEdges = 0 then
        failwith ("taxon not found in alignment: '"^taxName^"'")
      else if List.length outEdges > 1 then
        failwith ("taxon in alignment repeatedly: '"^taxName^"'")
      else (* pigeonhole principle *)
        List.hd outEdges)
    taxonMap

(* a like_aln is just the corresponding array of likelihood vectors *)
let like_aln_of_align seq_type align =
  let like_fun =
    match seq_type with
    | Alignment.Nucleotide_seq -> Nuc_models.lv_of_nuc
    | Alignment.Protein_seq -> Prot_models.lv_of_aa
  in
  Array.map
    (fun (_, seq) ->
      Array.map like_fun (StringFuns.to_char_array seq))
    align


(* getting emperical frequencies from alignments
 *)
let emper_freq nstates like_map align =
  let no_missing_normed =
    CharMap.remove '-' (
    CharMap.remove '?' ( (* we don't remove 'X'... *)
    CharMap.map (
      fun like_vect ->
        Linear_utils.alloc_l1_normalize like_vect) like_map)) in
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
  Linear_utils.l1_normalize total;
  (* Format.fprintf Format.std_formatter "%a@." Linear_utils.ppr_gsl_vector total; *)
  total

let list_of_any_file fname =
  let has_suffix suffix = Filename.check_suffix fname suffix in
  if has_suffix ".fasta" || has_suffix ".fa" then
    Fasta.of_file fname
  else if has_suffix ".sth" || has_suffix ".sto" then
    Stockholm.of_file fname
  else
    failwith ("unfamiliar suffix on " ^ fname)

let uppercase_list l = List.map Alignment.pair_uppercase l
let upper_list_of_any_file fname = uppercase_list (list_of_any_file fname)

let aln_of_any_file fname = Array.of_list (list_of_any_file fname)
let upper_aln_of_any_file fname = Alignment.uppercase (aln_of_any_file fname)

let identity s1 s2 =
  let s1' = StringFuns.to_char_array s1
  and s2' = StringFuns.to_char_array s2 in
  let num, denom = ArrayFuns.fold_left2
    (fun (num, denom) c1 c2 ->
      if c1 = '-' || c2 = '-' then
        num, denom
      else
        num + (if c1 = c2 then 1 else 0), succ denom)
    (0, 0)
    s1'
    s2'
  in
  (float_of_int num) /. (float_of_int denom), denom

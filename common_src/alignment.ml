exception Unknown_format of string

open Ppatteries

(* NOTE: zero is the beginning of the alignment! *)
(* NOTE: strings have maximum length of 16777211 (on stoke) *)

(* alignments are arrays of (name, seq)'s. better than hashtables because
 * numbers are useful *)

type t = (string * string) array

type seq_type = Nucleotide_seq | Protein_seq
let nstates_of_seq_type = function | Nucleotide_seq -> 4 | Protein_seq -> 20
let seq_type_to_str = function
    | Nucleotide_seq -> "nucleotide" | Protein_seq -> "protein"

(* ***** utils ***** *)
  (* string_break: break str in to chunks of length chunk_len *)
let string_break str chunk_len =
  let rec aux next_str =
    let next_len = String.length next_str in
    if next_len <= chunk_len then
      [ next_str ] (* terminate *)
    else
      ( String.sub next_str 0 chunk_len ) :: (
        aux ( String.sub next_str chunk_len ( next_len - chunk_len ) ) )
  in
  aux str

(* ***** basics ***** *)

let get_name align i = fst (align.(i))
let get_seq align i = snd (align.(i))

let iteri = Array.iteri

let get_name_arr align = Array.map fst align
let forget_names align = Array.map snd align

let same_lengths_unnamed align =
  if align = [||] then true
  else (
    let lengths = Array.map String.length align in
    Array.fold_left (
      fun same_so_far len ->
        same_so_far && ( len = lengths.(0) )
    ) true lengths
  )

let same_lengths align = same_lengths_unnamed ( forget_names align )

let n_seqs align = Array.length align

let length align =
  if n_seqs align = 0 then 0
  else if same_lengths align then String.length (get_seq align 0)
  else failwith "length: not all same length"

let pair_uppercase (name, seq) = (name, String.uppercase seq)
let uppercase aln = Array.map pair_uppercase aln

let list_of_any_file fname =
  let has_suffix suffix = Filename.check_suffix fname suffix in
  if has_suffix ".fasta" || has_suffix ".fa" then
    Fasta.of_file fname
  else if has_suffix ".sth" || has_suffix ".sto" then
    Stockholm.of_file fname
  else
    failwith ("unfamiliar suffix on " ^ fname)

let uppercase_list l = List.map pair_uppercase l
let upper_list_of_any_file fname = uppercase_list (list_of_any_file fname)

let aln_of_any_file fname = Array.of_list (list_of_any_file fname)
let upper_aln_of_any_file fname = uppercase (aln_of_any_file fname)

let to_map_by_name aln =
  Array.fold_right
    (fun (name, seq) m ->
      if StringMap.mem name m then
        failwith ("name "^name^" duplicated in alignment!");
      StringMap.add name seq m)
    aln
    StringMap.empty

(* alternate, for wrapped fasta
   List.iter
     (fun line -> Printf.fprintf ch "%s\n" line)
     (string_break seq 60); *)
let write_fasta_line ch (name, seq) =
  Printf.fprintf ch ">%s\n" name;
  Printf.fprintf ch "%s\n" seq

let to_fasta align fname =
  let ch = open_out fname in
  Array.iter (write_fasta_line ch) align;
  close_out ch

let to_phylip align fname =
  let ch = open_out fname in
  Printf.fprintf ch "%d %d\n" (n_seqs align) (length align);
  Array.iter (
    fun (name, seq) ->
      Printf.fprintf ch "%s %s\n" (StringFuns.left_pad 14 ' ' name) seq;
  ) align;
  close_out ch

let stack align1 align2 =
  let a = Array.append align1 align2 in
  if same_lengths a then a
  else failwith "stack: alignment not rectangular!"

(* string_mask: mask is a bool array whose ith elt tells if we should include
 * that char into the output *)
let string_mask mask str =
  assert(Array.length mask = String.length str);
  StringFuns.of_char_array (
    Array.filteri (fun i _ -> mask.(i)) (StringFuns.to_char_array str))

(* mask_align : mask the alignment *)
let mask_align mask align =
  Array.map (fun (name, seq) -> (name, string_mask mask seq)) align

(* XXX *)

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

(* make_aln_index_map: make a map which maps from the node number to the row
 * number of the alignment. *)
let make_aln_index_map taxon_map aln_name_arr =
  let n_tree = IntMap.cardinal taxon_map
  and n_aln = Array.length aln_name_arr in
  if n_tree <> n_aln then
    failwith
      (Printf.sprintf "tree has %d taxa, and ref align has %d." n_tree n_aln);
  check_for_repeats aln_name_arr;
  IntMap.map
    (fun tax_name ->
      match ArrayFuns.find_all_indices tax_name aln_name_arr with
        | [idx] -> idx
        | [] -> failwith ("taxon not found in alignment: '"^tax_name^"'")
        | _ -> failwith ("taxon in alignment repeatedly: '"^tax_name^"'"))
    taxon_map

(* a like_aln is just the corresponding array of likelihood vectors *)
let like_aln_of_align seq_type align =
  let like_fun =
    match seq_type with
    | Nucleotide_seq -> Nuc_models.lv_of_nuc
    | Protein_seq -> Prot_models.lv_of_aa
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
  (if denom = 0 then 0. else (float_of_int num) /. (float_of_int denom)),
  denom

let informative = function '?' | '-' -> false | _ -> true

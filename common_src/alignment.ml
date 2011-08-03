exception Unknown_format of string

open Batteries
open Fam_batteries
open MapsSets

(* NOTE: zero is the beginning of the alignment! *)
(* NOTE: strings have maximum length of 16777211 (on stoke) *)

(* alignments are arrays of (name, seq)'s. better than hashtables because
 * numbers are useful *)

type t = (string * string) array

type seq_type = Nucleotide_seq | Protein_seq
let nstates_of_seq_type = function | Nucleotide_seq -> 4 | Protein_seq -> 20

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

let array_filteri filter_fun a =
  let elts = ref [] in
  for i=0 to (Array.length a)-1 do
    if filter_fun i a.(i) then elts := a.(i)::!elts;
  done;
  Array.of_list (List.rev !elts)

(* string_mask: mask is a bool array whose ith elt tells if we should include
 * that char into the output *)
let string_mask mask str =
  assert(Array.length mask = String.length str);
  StringFuns.of_char_array (
    array_filteri (fun i _ -> mask.(i)) (StringFuns.to_char_array str))

(* mask_align : mask the alignment *)
let mask_align mask align =
  Array.map (fun (name, seq) -> (name, string_mask mask seq)) align

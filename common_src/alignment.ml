(* Copyright (C) 2009-10  Frederick A Matsen.
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

exception Unknown_format of string

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
  (* string_break: break str in to chunks of length chunkLen *)
let string_break str chunkLen = 
  let rec aux nextStr = 
    let nextLen = String.length nextStr in
    if nextLen <= chunkLen then
      [ nextStr ] (* terminate *)
    else
      ( String.sub nextStr 0 chunkLen ) :: ( 
        aux ( String.sub nextStr chunkLen ( nextLen - chunkLen ) ) )
  in
  aux str

(* ***** basics ***** *)

let get_name align i = fst (align.(i))
let get_seq align i = snd (align.(i))

let iteri = Array.iteri

let getNameArr align = Array.map fst align
let forgetNames align = Array.map snd align

let sameLengthsUnnamed align = 
  if align = [||] then true
  else (
    let lengths = Array.map String.length align in
    Array.fold_left ( 
      fun sameSoFar len ->
        sameSoFar && ( len = lengths.(0) )
    ) true lengths
  )

let sameLengths align = sameLengthsUnnamed ( forgetNames align )

let n_seqs align = Array.length align

let length align = 
  if n_seqs align = 0 then 0
  else if sameLengths align then String.length (get_seq align 0)
  else failwith "length: not all same length"

let uppercase aln = 
  Array.map (fun (name, seq) -> (name, String.uppercase seq)) aln

let to_map_by_name aln = 
  Array.fold_right 
    (fun (name, seq) m -> 
      if StringMap.mem name m then
        failwith ("name "^name^" duplicated in alignment!");
      StringMap.add name seq m) 
    aln
    StringMap.empty


(* ***** reading alignments ***** *)

(*
# Alignment.name_of_fasta_header ">dfd adfad" ;;
- : string = "dfd"
# Alignment.name_of_fasta_header ">dfd";;
- : string = "dfd"
*)
let name_of_fasta_header s = 
  let last = 
    try String.index s ' ' with 
    | Not_found -> String.length s
  in
  String.sub s 1 (last-1)

let read_fasta fname = 
  (* first count the number of careted lines in the alignment *)
  let ch = open_in fname in
  let is_name s = s.[0] = '>' in
  let n_seqs = ref 0 in
  (* count the number of entries *)
  let () = 
    try 
      while true do if is_name (input_line ch) then incr n_seqs done;
    with
    | End_of_file -> ()
  in
  (* read fasta entries *)
  let a = Array.make (!n_seqs) ("","") in
  seek_in ch 0;
  let count = ref (-1) in
  let () = 
    try
      while true do
        let line = input_line ch in
        if is_name line then begin
          incr count;
          assert(!count < !n_seqs);
          (* we have a new current sequence *)
          a.(!count) <- (name_of_fasta_header line,"");
        end
        else begin
          let (name,seq) = a.(!count) in
          (* append to current sequence *)
          a.(!count) <- (name, seq^line);
        end
      done;
    with
    | End_of_file -> ()
  in
  close_in ch;
  a

(* read an alignment, type unspecified *)
let read_align fname = 
  let suffix = Str.replace_first (Str.regexp ".*\\.") "" fname in
  if suffix = "fasta" || suffix = "fa" then read_fasta fname 
  else begin
    print_endline "This program only accepts FASTA files with .fa or .fasta suffix";
    raise (Unknown_format suffix)
  end

(* alternate, for wrapped fasta
   List.iter 
     (fun line -> Printf.fprintf ch "%s\n" line)
     (string_break seq 60); *)
let write_fasta_line ch (name, seq) = 
  Printf.fprintf ch ">%s\n" name;
  Printf.fprintf ch "%s\n" seq

let toFasta align fname = 
  let ch = open_out fname in
  Array.iter (write_fasta_line ch) align;
  close_out ch

let toPhylip align fname = 
  let ch = open_out fname in
  Printf.fprintf ch "%d %d\n" (n_seqs align) (length align);
  Array.iter (
    fun (name, seq) ->
      Printf.fprintf ch "%s %s\n" (StringFuns.left_pad 14 ' ' name) seq;
  ) align;
  close_out ch

let toMatrixUnnamed alignUnnamed = 
  if not (sameLengthsUnnamed alignUnnamed) then
    failwith "toColumns: alignment not rectangular!"
  else if alignUnnamed = [||] then
    [||]
  else (
      Array.map StringFuns.to_char_array (alignUnnamed)
  )

let toMatrix align = toMatrixUnnamed (forgetNames align)
(* let toColMatrix align = Biobase.matrix_transpose (toMatrix align) *)

(* let findBases col = ListFuns.uniques (Array.to_list col) *)
(* let nBases col = List.length (findBases col) *)

let stack align1 align2 = 
  let a = Array.append align1 align2 in
  if sameLengths a then a
  else failwith "stack: alignment not rectangular!"

let filter_zero_length align = 
  Array.of_list
    (List.filter
      (fun (_,seq) -> seq <> "")
      (Array.to_list align))

let array_filteri filterFun a =
  let elts = ref [] in
  for i=0 to (Array.length a)-1 do
    if filterFun i a.(i) then elts := a.(i)::!elts;
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

(* pickColumns: which is a bool array of the same length determining if that
 * column is included *)
let pickColumns which align = 
  assert(Array.length which = length align);
  Array.map (
    fun (name, seq) -> 
      (name, string_mask which seq)
  ) align

let colIsConstant col = 
  Array.fold_left (
    fun soFar x ->
      soFar && ( x = col.(0) )
  ) true col

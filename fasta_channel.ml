(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * functions to iterate over a fasta file
*)

exception Beginning_of_file

let whitespace_regexp = Str.regexp "^[ \\t]*$"
let whitespace_match s = Str.string_match whitespace_regexp s 0

let fasta_name_regexp = Str.regexp "^>\\(.*\\)"
let fasta_name_match s = Str.string_match fasta_name_regexp s 0
let extract_name l = 
  assert(fasta_name_match l);
  Str.matched_group 1 l

let of_fname = open_in 
let close = close_in

let complete_rewind ch = seek_in ch 0

let rewind ch n_chars = 
  let new_pos = (pos_in ch)-n_chars in
  if new_pos <= 0 then raise Beginning_of_file;
  seek_in ch new_pos

(* return the previous character, if it exists, and don't move the file position
 *)
let prev_char ch = 
  if 0 = pos_in ch then None
  else begin
    rewind ch 1;
    Some (input_char ch)
  end

let rec rewind_to_beginning_of_line ch = 
  if pos_in ch <= 1 then complete_rewind ch
  else match prev_char ch with
  | None -> ()
  | Some c -> 
      if c <> '\n' then begin
        rewind ch 1;
        rewind_to_beginning_of_line ch
      end

(* rewind one line. will raise Beginning_of_file if at beginning. *)
let rewind_line ch = 
  rewind ch 1;
  rewind_to_beginning_of_line ch

(* return the next fasta name and move the file position to just after the name.
 * will raise End_of_file if there are no more names *)
let rec next_name ch = 
  let line = input_line ch in
  if fasta_name_match line then extract_name line
  else next_name ch

(* keep on adding strings to accu until a name is encountered *)
let rec concat_until_name ch accu = 
  try
    let line = input_line ch in
    if fasta_name_match line then begin rewind_line ch; accu end
    else concat_until_name ch (accu^line)
  with
  | End_of_file -> accu

(* assuming that the file position is currently at a fasta name, return the next
 * (name, seq) pair. will raise End_of_file if there are no more names *)
let rec next_named_seq ch = 
  let name_line = input_line ch in
  if fasta_name_match name_line then 
    (extract_name name_line, concat_until_name ch "")
  else
    invalid_arg (name_line^" does not appear to be a fasta name in Fasta_channel. perhaps we got off track.")

(* *** primary public interface ***
 * here we give functions for itering and iteri-ing over names and (name,seq)
 * pairs.
 * *)
let gen_iteri next_fun f ch = 
  let rec aux i = f i (next_fun ch); aux (i+1) in
  try aux 0 with | End_of_file -> complete_rewind ch

let gen_iter next_fun f = gen_iteri next_fun (fun _ x -> f x)

let name_iteri f ch = gen_iteri next_name f ch
let name_iter f ch = gen_iter next_name f ch

let named_seq_iteri f ch = gen_iteri next_named_seq f ch
let named_seq_iter f ch = gen_iter next_named_seq f ch


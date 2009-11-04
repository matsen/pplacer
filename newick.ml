(* pplacer v0.3. Copyright (C) 2009  Frederick A
 * Matsen.warn_about_duplicate_names combined;
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * this should probably be called newick_gtree.
 *)

open MapsSets
open Fam_batteries

(* use *)

let compare t1 t2 = Gtree.compare Newick_bark.compare t1 t2

(* output *)

let string_of_bark t id = 
    match Gtree.get_bark_opt t id with
    | Some b -> b#to_newick_string
    | None -> ""

let to_string_gen f t = 
    (Gtree.recur
      (fun id below ->
        "("^(String.concat "," below)^")"^(f t id))
      (f t)
      t)^";"

let to_string t = to_string_gen string_of_bark t
let to_numbered_string t =
  to_string_gen (fun _ id -> string_of_int id) t

let write ch t = Printf.fprintf ch "%s\n" (to_string t)

let tree_list_to_file trees fname = 
  let ch = open_out fname in
  List.iter (write ch) trees;
  close_out ch

(* input *)

(* count the number of occurrences of char c in str *)
let count_n_occurrences c str = 
  let count = ref 0 in
  String.iter (fun d -> if c = d then incr count) str;
  !count

let check_string s = 
  let n_open = count_n_occurrences '(' s 
  and n_closed = count_n_occurrences ')' s in
  if n_open <> n_closed then
    Printf.printf "warning: %d open parens and %d closed parens\n" n_open n_closed;
  ()

let of_lexbuf lexbuf = 
  try
    Newick_parser.tree Newick_lexer.token lexbuf
  with 
  | Parsing.Parse_error -> failwith "couldn't parse tree!"

let of_string s = 
  check_string s;
  of_lexbuf 
  (Lexing.from_string (Str.replace_first (Str.regexp ");") "):0.;" s))

let of_file fname = 
  match
    List.filter 
      (fun line -> 
        not (Str.string_match (Str.regexp "^[ \t]*$") line 0)) 
      (File_parsing.string_list_of_file fname)
  with
    | [] -> failwith ("empty file in "^fname)
    | [s] -> of_string s
    | _ -> failwith ("expected a single tree on a single line in "^fname)

let list_of_file fname =
  List.map of_string (File_parsing.string_list_of_file fname)

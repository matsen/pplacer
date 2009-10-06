(* pplacer v0.3. Copyright (C) 2009  Frederick A
 * Matsen.warn_about_duplicate_names combined;
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets
open Stree
open Fam_batteries

(* output *)

(*
Unquoted labels may not contain blanks, parentheses, square brackets,
single_quotes, colons, semicolons, or commas.
Underscore characters in unquoted labels are converted to blanks.
*)
let need_quoting = 
  Array.to_list (StringFuns.to_char_array " ()[]':;,")

let if_quote_label label = 
  try
    String.iter 
      (fun c -> if List.mem c need_quoting then raise Exit)
      label;
    false
  with
  | Exit -> true

let prepare_label label = 
  if if_quote_label label then "'"^label^"'" else label

(* nodeInfo_entryToStr :
 * this will produce funny output if there is bootstrap info for a taxon (which
 * is nonsensical)
 * *)
let nodeInfo_entryToStr ni i = 
  let entryToStr toStrFn map = 
    if IntMap.mem i map then toStrFn (IntMap.find i map)
    else ""
  in
  (entryToStr prepare_label ni.taxon)^
  (entryToStr gstring_of_float ni.boot)^(
    if IntMap.mem i ni.bl then (":"^(gstring_of_float (IntMap.find i ni.bl)))
    else "")

let print_tree_info tree = 
  List.iter
    (fun id -> 
      Printf.printf "%d:\t%s\n" id (nodeInfo_entryToStr tree.info id))
    (Stree.collect_node_numbers tree.tree)

let to_newick_gen entry_to_str istree = 
  let rec aux = function
    | Node(i, tL) ->
        "("^(String.concat "," (List.map aux tL))^")"^
        (entry_to_str istree.info i)
    | Leaf i -> entry_to_str istree.info i
  in
  (aux istree.tree)^";"

let to_newick = to_newick_gen nodeInfo_entryToStr
let to_newick_numbered = 
  to_newick_gen (fun _ i -> string_of_int i)

let write_newick ch istree = 
  Printf.fprintf ch "%s\n" (to_newick istree)

let rec ppr_gen_stree ppr_node ff = function
  | Node(i, tL) ->
      Format.fprintf ff "@[(";
      Ppr.ppr_gen_list_inners "," (ppr_gen_stree ppr_node) ff tL;
      Format.fprintf ff ")";
      ppr_node ff i;
      Format.fprintf ff "@]"
  | Leaf(i) -> ppr_node ff i

let ppr_stree = ppr_gen_stree Format.pp_print_int

let ppr_info_stree ff istree = 
  ppr_gen_stree (
    fun ff i -> Format.fprintf ff "%s" (nodeInfo_entryToStr istree.info i)
  ) ff istree.tree

let make_numbered_tree tree =
  Stree.make_boot_node_num 
    (Stree.inform_stree 
      (Stree.get_tree tree)
      {(Stree.get_info tree) with 
      Stree.taxon = 
        (IntMap.map 
        (fun s -> s^"@")
        ((Stree.get_info tree).Stree.taxon))})

  (* input *)

(* count_n_occurrences :
  * count the number of occurrences of c in str
  * *)
let count_n_occurrences c str = 
  let count = ref 0 in
  String.iter (fun d -> if c = d then incr count) str;
  !count

let problematic_regexes =
  List.map (fun (r, msg) -> (Str.regexp r, msg)) 
  [
    ".*;;", "double semicolon";
    (* "([^(),]*REMOVE)", "degree two node" *)
  ]

let check_newick_str s = 
  (*
  List.iter (
    fun (regex, msg) ->
      try
        let _ = Str.search_forward regex s 0 in 
        (* found a problematic string *)
        print_endline ("newick parsing warning: "^msg)
      with
      | Not_found -> ()
  ) problematic_regexes;
  *)
  let n_open = count_n_occurrences '(' s 
  and n_closed = count_n_occurrences ')' s in
  if n_open <> n_closed then
    Printf.printf "warning: %d open parens and %d closed parens\n" n_open n_closed;
  ()

let of_newick_lexbuf lexbuf = 
  try
    Stree_parser.tree Stree_lexer.token lexbuf
  with 
  | Parsing.Parse_error -> failwith "couldn't parse tree!"

let of_newick_str s = 
  check_newick_str s;
  of_newick_lexbuf 
  (Lexing.from_string (Str.replace_first (Str.regexp ");") "):0.;" s))

let of_newick_file fname = 
  match
    List.filter 
      (fun line -> 
        not (Str.string_match (Str.regexp "^[ \t]*$") line 0)) 
      (Common_base.stringListOfFile fname)
  with
    | [] -> failwith ("empty file in "^fname)
    | [s] -> of_newick_str s
    | _ -> failwith ("expected a single tree on a single line in "^fname)

let listOfNewickFile fname =
  List.map of_newick_str (Common_base.stringListOfFile fname)

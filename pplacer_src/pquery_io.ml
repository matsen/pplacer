(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * reading and writing pqueries.
 *)

open Fam_batteries
open MapsSets

let no_seq_str = "<sequence not loaded>"

let space_split = Str.split (Str.regexp "[ \t]+")


(* ***** WRITING ***** *)

let write ch pq =
  Printf.fprintf ch ">%s\n" (String.concat " " (Pquery.namel pq));
  Printf.fprintf ch "%s\n" (Pquery.seq pq);
  List.iter
    (fun p ->
      Printf.fprintf ch "%s\n" (Placement.to_str p))
    (Pquery.place_list pq)

(* convert to a string list list appropriate for using with the Csv module. *)
let to_csv_strl pq =
  let qname = String.concat " " (Pquery.namel pq) in
  ListFuns.mapi
    (fun i p -> qname::(string_of_int i)::(Placement.to_csv_strl p))
    (Pquery.place_list pq)

let to_json pq =
  let tbl = Hashtbl.create 4 in
  let namel = List.map (fun s -> Jsontype.String s) (Pquery.namel pq) in
  Hashtbl.add tbl "n" (Jsontype.Array (Array.of_list namel));
  Hashtbl.add tbl "p" (Jsontype.Array (Array.map Placement.to_json (Array.of_list (Pquery.place_list pq))));
  Jsontype.Object tbl


(* ***** READING ***** *)

let parse_pquery ?load_seq:(load_seq=true) = function
  | header::seq::places ->
      let my_seq = if load_seq then seq else no_seq_str in
      Pquery.make_ml_sorted
      ~namel:(space_split (Alignment.remove_fasta_gt header))
      ~seq:my_seq
      (List.map Placement.placement_of_str places)
  | _ ->
      invalid_arg "problem with place file. missing sequence data?"

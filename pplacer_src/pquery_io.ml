(* reading and writing pqueries.
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

let to_json has_classif pq =
  let tbl = Hashtbl.create 4 in
  let namel = List.map (fun s -> Jsontype.String s) (Pquery.namel pq) in
  Hashtbl.add tbl "n" (Jsontype.Array namel);
  Hashtbl.add tbl "p" (Jsontype.Array (
    List.map
      (Placement.to_json has_classif)
      (Pquery.place_list pq)));
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

let of_json fields o =
  let tbl = Jsontype.obj o in
  let namel = List.map Jsontype.string (Jsontype.array (Hashtbl.find tbl "n")) in
  let pa = List.map (Placement.of_json fields) (Jsontype.array (Hashtbl.find tbl "p")) in
  Pquery.make_ml_sorted ~namel ~seq:no_seq_str pa


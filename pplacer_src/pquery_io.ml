(* reading and writing pqueries.
 *)

open Ppatteries

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
  List.mapi
    (fun i p -> qname::(string_of_int i)::(Placement.to_csv_strl p))
    (Pquery.place_list pq)

let to_json pq =
  let tbl = Hashtbl.create 4 in
  Hashtbl.add
    tbl
    "nm"
    (Jsontype.Array
       (List.map
          (fun (n, m) -> Jsontype.Array [Jsontype.String n; Jsontype.Float m])
          (Pquery.namlom pq)));
  let maps = Pquery.place_list pq |> List.map Placement.to_json in
  List.enum maps |> Enum.map StringMap.keys |> Enum.flatten |> StringSet.of_enum,
  fun fields ->
    flip List.map maps (fun m ->
      Jsontype.Array
        (List.map (fun k -> StringMap.get k Jsontype.Null m) fields))
    |> (fun x -> Jsontype.Array x)
    |> Hashtbl.add tbl "p";
    Jsontype.Object tbl


(* ***** READING ***** *)

let remove_fasta_gt s =
  assert(s.[0] == '>');
  String.sub s 1 ((String.length s)-1)

let parse_pquery ?load_seq:(load_seq=true) = function
  | header::seq::places ->
      let my_seq = if load_seq then seq else no_seq_str in
      Pquery.make_ml_sorted
      ~namlom:(Pquery.uniform_namel (space_split (remove_fasta_gt header)))
      ~seq:my_seq
      (List.map Placement.placement_of_str places)
  | _ ->
      invalid_arg "problem with place file. missing sequence data?"

let of_json fields o =
  let tbl = Jsontype.obj o in
  let namlom =
    if Hashtbl.mem tbl "n" then begin
      match Hashtbl.find tbl "n" with
        | Jsontype.String s ->
          [s,
           Hashtbl.find_option tbl "m"
           |> Option.map_default Jsontype.float 1.]
        | Jsontype.Array arr -> List.map (Jsontype.string &&& const 1.) arr
        | x -> Jsontype.unexpected x "string or string array"
    end else (* ... *)
    Hashtbl.find tbl "nm"
    |> Jsontype.array
    |> List.map
        (function
          | Jsontype.Array [Jsontype.String n; m] -> n, Jsontype.float m
          | _ -> failwith "malformed namlom in jplace file")
  in
  List.map
    (Placement.of_json fields)
    (Hashtbl.find tbl "p" |> Jsontype.array)
  |> Pquery.make_ml_sorted ~namlom ~seq:no_seq_str

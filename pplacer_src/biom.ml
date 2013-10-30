open Ppatteries

let split_file_regexp = Str.regexp "^\\(.+\\):\\(.+?\\)$"
let split_file s =
  if not (Str.string_match split_file_regexp s 0) then
    failwith "biom file provided with no tree";
  Str.matched_group 1 s, Str.matched_group 2 s

let safe_hashtbl_find h k =
  try Hashtbl.find h k with
  | Not_found -> failwith ("'"^k ^"' not found in the BIOM file!")

exception No_leaf of string

let () = Printexc.register_printer
  (function
    | No_leaf l ->
      Some (Printf.sprintf "leaf %s in BIOM file not found on provided tree" l)
    | _ -> None)

let of_tree_and_biom combined_filename =
  let tree, biom = split_file combined_filename in
  let gt = Newick_gtree.of_file tree
  and j = Jsontype.obj (Json.of_file biom) in
  let leaf_map = Newick_gtree.leaf_label_map gt
    |> IntMap.enum
    |> Enum.map swap
    |> StringMap.of_enum
  and get k = match Hashtbl.find_option j k with
    | Some x -> x
    | None -> failwith (Printf.sprintf "couldn't find key %S" k)
  in
  if get "type" <> Jsontype.String "OTU table" then
    failwith "only OTU table BIOM files are supported";
  let counts = match get "matrix_type" with
  | Jsontype.String "dense" ->
    get "data"
      |> Jsontype.array
      |> List.mapi (fun i l ->
        Jsontype.array l |> List.mapi (fun j x -> i, j, Jsontype.int x))
      |> List.flatten
  | Jsontype.String "sparse" ->
    get "data"
      |> Jsontype.array
      |> List.map
          (Jsontype.array |- List.enum |- map Jsontype.int |- Tuple3.of_enum)
  | _ -> failwith "OTU matrix must be either sparse or dense"
  and parse_id obj =
        safe_hashtbl_find (Jsontype.obj obj) "id" |> Jsontype.string in
  let parse_ids = get |- Jsontype.array |- List.map parse_id |- Array.of_list in
  let rows = parse_ids "rows"
  and columns = parse_ids "columns" in
  List.group (comparing Tuple3.second) counts |> List.map (fun ll ->
    let sample = columns.(List.hd ll |> Tuple3.second) in
    flip List.map ll (fun (row, _, count) ->
      let loc =
        try
          StringMap.find rows.(row) leaf_map
        with Not_found -> raise (No_leaf rows.(row))
      in
      Pquery.make_ml_sorted
        ~namlom:["pl_" ^ rows.(row), float_of_int count]
        ~seq:Pquery_io.no_seq_str
        [Placement.make_ml loc ~ml_ratio:1. ~log_like:0. ~dist_bl:0. ~pend_bl:0.
         |> Placement.add_pp ~post_prob:1. ~marginal_prob:1.])
    |> Placerun.make gt sample)

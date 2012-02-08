open Ppatteries

type t = Newick_bark.newick_bark Gtree.gtree


(* use *)

(* epsilon is the "error" allowed in the floating point comparisons, and
 * cmp_boot determines if the comparison should compare bootstraps. *)
let compare ?epsilon:(epsilon=0.) ?(cmp_edge_label = true) t1 t2 =
  Gtree.compare (Newick_bark.compare ~epsilon ~cmp_edge_label) t1 t2

let to_numbered t =
  Gtree.mapi_bark_map (fun i x -> x#to_numbered i) t

let make_edge_label_id t =
  Gtree.mapi_bark_map (fun i x -> x#set_edge_label (string_of_int i)) t

let node_labels_of_ids t =
  List.enum
  |- Enum.filter_map
      (Result.catch (identity &&& Gtree.get_node_label t)
       |- Result.to_option)
  |- IntMap.of_enum

(* Make a list of the names on the tree, including the internal nodes. *)
let node_label_map t =
  Gtree.node_ids t |> node_labels_of_ids t
(* The same, but only the leaves on the tree. *)
let leaf_label_map t =
  Gtree.leaf_ids t |> node_labels_of_ids t

let has_zero_bls t =
  List.fold_left
    (fun accum id -> accum || Gtree.get_bl t id = 0.)
    false
    (Stree.nonroot_node_ids (Gtree.get_stree t))

(* output *)

let string_of_bark ?(with_node_numbers = false) t id =
  match Gtree.get_bark_opt t id with
    | Some b -> b#to_newick_string (if with_node_numbers then Some id else None)
    | None when with_node_numbers -> Printf.sprintf "{%d}" id
    | None -> ""

let to_string_gen f t =
    (Gtree.recur
      (fun id below ->
        "("^(String.concat "," below)^")"^(f t id))
      (f t)
      t)^";"

let to_string ?with_node_numbers t =
  to_string_gen (string_of_bark ?with_node_numbers) t

let write ?with_node_numbers ch t =
  Printf.fprintf ch "%s\n" (to_string ?with_node_numbers t)

let tree_list_to_file ?with_node_numbers trees fname =
  let ch = open_out fname in
  List.iter (write ?with_node_numbers ch) trees;
  close_out ch

let to_file ?with_node_numbers t fname = tree_list_to_file ?with_node_numbers [t] fname

let ppr ff t =
  let ppr_bark ff id =
    Format.pp_print_string ff (string_of_bark t id) in
  let rec aux ff = function
    | Stree.Node(id, tL) ->
      Format.fprintf ff "@[(%a)%a@]"
        (Ppr.ppr_gen_list_inners "," aux) tL
        ppr_bark id
    | Stree.Leaf(id) ->
        ppr_bark ff id
  in
  Format.fprintf ff "%a;" aux (Gtree.get_stree t)


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
    dprintf "warning: %d open parens and %d closed parens\n" n_open n_closed;
  ()

let of_lexbuf ?(legacy_format = false) lexbuf =
  Newick_parse_state.node_num := (-1);
  Newick_parse_state.legacy_format := legacy_format;
  try
    Newick_parser.tree Newick_lexer.token lexbuf
  with
  | Parsing.Parse_error -> failwith "couldn't parse tree!"

let of_string ?legacy_format s =
  check_string s;
  try
    of_lexbuf
      ?legacy_format
      (Lexing.from_string s)
  with
  | Failure s -> failwith("problem parsing tree: "^s)

let of_file ?legacy_format fname =
  match
    List.filter
      (fun line ->
        not (Str.string_match (Str.regexp "^[ \t]*$") line 0))
      (File_parsing.string_list_of_file fname)
  with
    | [] -> failwith ("empty file in "^fname)
    | [s] -> of_string ?legacy_format s
    | _ -> failwith ("expected a single tree on a single line in "^fname)

let list_of_file fname =
  List.map of_string (File_parsing.string_list_of_file fname)

let consolidate gt =
  let bl = Gtree.get_bl gt in
  let open Stree in
  let rec aux parent = function
    | Node (i, [t]) -> aux (Some ((i, bl i) :: Option.default [] parent)) t
    | t ->
      let t', transm, barkm = match t with
        | Leaf _ -> t, IntMap.empty, IntMap.empty
        | Node (i, subtrees) ->
          List.fold_left
            (fun (ta, ba, ma) x ->
              let tx, bx, mx = aux None x in
              tx :: ta, IntMap.union bx ba, IntMap.union mx ma)
            ([], IntMap.empty, IntMap.empty)
            subtrees
          |> Tuple3.map1 (List.rev |- node i)
      in
      let i = top_id t' in
      let bark = Gtree.get_bark gt i in
      let transm', bl' = List.fold_left
        (fun (tma, bla) (j, jbl) -> IntMap.add j (i, bla) tma, bla +. jbl)
        (IntMap.add i (i, 0.) transm, bl i)
        (Option.default [] parent)
      in
      t', transm', IntMap.add i (bark#set_bl bl') barkm
  in
  let stree, transm, bark_map = Gtree.get_stree gt |> aux None in
  let gt', transm' = Gtree.gtree stree bark_map |> Gtree.renumber in
  gt', IntMap.map (flip IntMap.find transm' |> first) transm

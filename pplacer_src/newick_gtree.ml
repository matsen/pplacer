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

(* Make a list of the names on the tree, including the internal nodes. *)
let get_node_label_list t =
  let rec aux = function
    | id::l ->
        begin
          match (Gtree.get_bark t id)#get_node_label_opt with
          | Some s -> s::(aux l)
          | None -> aux l
        end
    | [] -> []
  in
  aux (Gtree.node_ids t)

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

let write ch t = Printf.fprintf ch "%s\n" (to_string t)

let tree_list_to_file trees fname =
  let ch = open_out fname in
  List.iter (write ch) trees;
  close_out ch

let to_file t fname = tree_list_to_file [t] fname

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

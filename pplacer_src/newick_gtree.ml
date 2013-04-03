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

let label_to_leaf_map gt map =
  let labels = leaf_label_map gt
    |> IntMap.enum
    |> Enum.map swap
    |> StringMap.of_enum
  in
  StringMap.enum map
    |> Enum.filter_map
        (fun (label, x) ->
          match StringMap.Exceptionless.find label labels with
          | Some i -> Some (i, x)
          | None -> None)
    |> IntMap.of_enum

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

let of_lexbuf ?(legacy_format = false) ?fname lexbuf =
  Newick_parse_state.node_num := (-1);
  Newick_parse_state.legacy_format := legacy_format;
  Sparse.wrap_of_fname_opt
    fname
    (Newick_parser.tree Newick_lexer.token)
    lexbuf

let of_string ?legacy_format ?fname s =
  check_string s;
  try
    of_lexbuf
      ?fname
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
    | [s] -> of_string ?legacy_format ~fname s
    | _ -> failwith ("expected a single tree on a single line in "^fname)

let of_refpkg_contents =
  Refpkg_parse.of_file_or_string of_file (of_string ?legacy_format:None)

let list_of_file fname =
  List.map (of_string ~fname) (File_parsing.string_list_of_file fname)

let gen_add_zero_root_bl empty gt =
  let bm = Gtree.get_bark_map gt
  and i = Gtree.top_id gt in
  let bark = IntMap.get i empty bm in
  let bark' = match bark#get_bl_opt with
    | None -> bark#set_bl 0.
    | Some _ -> bark
  in
  IntMap.add i bark' bm |> Gtree.set_bark_map gt

let add_zero_root_bl =
  gen_add_zero_root_bl
    (new Newick_bark.newick_bark `Empty)

(* Given a newick gtree, collapse all nodes with only one child, so that the
 * resulting tree is always at least bifurcating. The result is also
 * renumbered through Gtree.renumber. The translation map returned is similar
 * to that returned by Gtree.renumber, except that it maps from the node
 * number to a pair of (new node number, increased distal branch length). *)
let consolidate gt =
  let gt = add_zero_root_bl gt in
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

let prune_to_pql should_prune ?(placement_transform = const identity) gt =
  let open Stree in
  let st = Gtree.get_stree gt
  and bl = Gtree.get_bl gt
  and name = Gtree.get_node_label gt in
  let rec aux attachment_opt = function
    | Leaf i when should_prune i ->
      let loc, pend_bl = Option.get attachment_opt |> second ((+.) (bl i)) in
      let pq = Pquery.make_ml_sorted
        ~namlom:[name i, 1.]
        ~seq:Pquery_io.no_seq_str
        [Placement.make_ml loc ~ml_ratio:1. ~log_like:0. ~dist_bl:0. ~pend_bl
         |> Placement.add_pp ~post_prob:1. ~marginal_prob:1.
         |> placement_transform i]
      in
      None, [pq]
    | Leaf _ as l -> Some l, []
    | Node (i, subtrees) ->
      let pruned = should_prune i in
      let attachment_opt' = match attachment_opt with
        | _ when not pruned -> Some (i, 0.)
        | Some (loc, pend_bl) -> Some (loc, pend_bl +. bl i)
        | None -> failwith "whole tree pruned"
      in
      List.fold_left
        (fun (st_accum, pql_accum) t ->
          let t_opt, pql = aux attachment_opt' t in
          maybe_cons t_opt st_accum, List.append pql pql_accum)
        ([], [])
        subtrees
      |> first (if pruned then const None else node i |- some)
  in
  let gt', pql = aux None st |> first (Option.get |- Gtree.set_stree gt) in
  let replace_root_placement =
    let open Placement in
    let top, location = match Gtree.get_stree gt' with
      | Node (top, subtree :: _) -> top, top_id subtree
      | _ -> failwith "trimmed tree's root is not a node with >1 subtree"
    in
    let distal_bl = Gtree.get_bl gt' location in
    fun p -> if p.location = top then {p with location; distal_bl} else p
  in
  gt',
  List.map
    (Pquery.apply_to_place_list (List.map replace_root_placement))
    pql

let expand_multifurcation_to_bifurcation ?(fresh_bark = const (Newick_bark.empty#set_bl 0.)) gt =
  let open Stree in
  let st = Gtree.get_stree gt
  and bm = ref (Gtree.get_bark_map gt) in
  let last_id = ref (max_id st) in
  let next () =
    incr last_id;
    bm := IntMap.add !last_id (fresh_bark !last_id) !bm;
    !last_id
  in
  let rec aux = function
    | Node (i, top :: (_ :: _ :: _ as rest)) ->
      node i [aux top; aux (node (next ()) rest)]
    | Node (i, subtrees) ->
      node i (List.map aux subtrees)
    | Leaf _ as l -> l
  in
  let st' = aux st in
  Gtree.gtree st' !bm

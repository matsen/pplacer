(* The bark for newick gtrees, i.e. trees that only have bl, name, and boot.
*)

open Ppatteries

exception No_bl
exception No_node_label
exception No_edge_label

let ppr_opt_node_labeld node_label ppr_val ff = function
  | Some x -> Format.fprintf ff " %s = %a;@," node_label ppr_val x
  | None -> ()

let write_something_opt write_it ch = function
  | Some x -> write_it ch x
  | None -> ()

let needs_quotes = Str.regexp "\\(\\([ \t\n(){}:;,]\\|\\[\\|]\\)\\|\\(['\\\\]\\)\\)"
let quote_label s =
  let replaced = ref false in
  let s' = Str.global_substitute
    needs_quotes
    (fun s ->
      replaced := true;
      match Sparse.first_match [2; 3] s with
        | 2, s -> s
        | 3, s -> "\\" ^ s
        | _, _ -> invalid_arg "quote_label")
    s
  in if !replaced then Printf.sprintf "'%s'" s' else s'

let maybe_float = Option.bind (Result.catch float_of_string |- Result.to_option)

class newick_bark arg =

  let (bl, node_label, edge_label) =
    match arg with
    | `Empty -> (None, None, None)
    | `Of_bl_node_edge_label (bl, node_label, edge_label) -> (bl, node_label, edge_label)
  in

  object (self)
    val bl = bl
    val node_label = node_label
    val edge_label = edge_label

    method get_bl_opt = bl
    method get_bl =
      match bl with | Some x -> x | None -> raise No_bl
    method set_bl_opt xo = {< bl = xo >}
    method set_bl (x:float) = {< bl = Some x >}

    method get_node_label_opt = node_label
    method get_node_label =
      match node_label with | Some s -> s | None -> raise No_node_label
    method set_node_label_opt so = {< node_label = so >}
    method set_node_label s = {< node_label = Some s >}

    method get_edge_label_opt = edge_label
    method get_edge_label =
      match edge_label with | Some x -> x | None -> raise No_edge_label
    method set_edge_label_opt xo = {< edge_label = xo >}
    method set_edge_label x = {< edge_label = Some x >}

    method get_confidence_name_opt is_leaf =
      match maybe_float node_label with
        | c when not is_leaf -> c, None
        | _ -> maybe_float edge_label, node_label

    method to_newick_string node_number =
      Printf.sprintf "%s%s%s%s"
        (Option.map_default quote_label "" node_label)
        (Option.map_default (Printf.sprintf ":%g") "" bl)
        (Option.map_default (Printf.sprintf "{%d}") "" node_number)
        (Option.map_default (quote_label |- Printf.sprintf "[%s]") "" edge_label)

    method private ppr_inners ff =
      ppr_opt_node_labeld "bl" Format.pp_print_float ff bl;
      ppr_opt_node_labeld "node_label" Format.pp_print_string ff node_label;
      ppr_opt_node_labeld "edge_label" Format.pp_print_string ff edge_label

    method ppr ff =
      Format.fprintf ff "@[{%a}@]" (fun ff () -> self#ppr_inners ff) ()

    method to_xml is_leaf = begin
      let confidence, name = self#get_confidence_name_opt is_leaf in
      []
      |> maybe_map_cons (Myxml.tag "name") name
      |> maybe_map_cons (Printf.sprintf "%g" |- Myxml.tag "branch_length") bl
      |> maybe_map_cons
          (Printf.sprintf "%g"
           |- Myxml.tag "confidence" ~attrs:["type", "confidence"])
          confidence
      |> List.rev
    end

    method to_numbered id =
      {<
        node_label = Some (Option.map_default (Printf.sprintf "@%s") "" node_label);
        edge_label = Some (string_of_int id);
      >}

  end

type t = newick_bark

let empty = new newick_bark `Empty

let float_approx_compare epsilon x y =
  let diff = x -. y in
  if abs_float diff <= epsilon then 0
  else if diff < 0. then -1
  else 1

let floato_approx_compare epsilon a b =
  match (a, b) with
  | (Some x, Some y) -> float_approx_compare epsilon x y
  | (a, b) -> Pervasives.compare a b

let compare ?epsilon:(epsilon=0.) ?cmp_edge_label:(cmp_edge_label=true) b1 b2 =
  let fc = floato_approx_compare epsilon in
  try
    raise_if_different fc b1#get_bl_opt b2#get_bl_opt;
    raise_if_different compare b1#get_node_label_opt b2#get_node_label_opt;
    if cmp_edge_label then
      raise_if_different compare b1#get_edge_label_opt b2#get_edge_label_opt;
    0
  with
  | Different c -> c

let map_find_loose id m =
  if IntMap.mem id m then IntMap.find id m
  else new newick_bark `Empty

let map_set_bl id bl m =
  IntMap.add id ((map_find_loose id m)#set_bl bl) m

let map_set_node_label id node_label m =
  IntMap.add id ((map_find_loose id m)#set_node_label node_label) m

let map_set_edge_label id edge_label m =
  IntMap.add id ((map_find_loose id m)#set_edge_label edge_label) m

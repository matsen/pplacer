(* Our basic tree data structure without information.
 *
 * *)
open Ppatteries

type stree = Node of int * stree list | Leaf of int
type t = stree

let node i tL = Node(i, tL)
let leaf i = Leaf i

let of_id i = function
  | Node(_, tL) -> Node(i, tL)
  | Leaf(_) -> Leaf(i)

let rec n_taxa = function
  | Node(_,tL) -> List.fold_left ( + ) 0 (List.map n_taxa tL)
  | Leaf(_) -> 1

(* Number of non-root edges. *)
let n_edges stree =
  let rec aux = function
    | Node(_,tL) -> List.fold_left ( + ) 1 (List.map aux tL)
    | Leaf(_) -> 1
  in
  (aux stree) - 1

let rec node_ids_aux = function
  | Node(i,tL) -> i :: (List.flatten (List.map node_ids_aux tL))
  | Leaf(i) -> [i]

let node_ids stree = List.sort compare (node_ids_aux stree)
let nonroot_node_ids stree =
  try List.sort compare (List.tl (node_ids_aux stree)) with
  | Failure _ -> invalid_arg "nonroot_node_ids"

let rec leaf_ids = function
  | Node(_,tL) -> List.flatten (List.map leaf_ids tL)
  | Leaf(i) -> [i]

let top_id = function
  | Node(i, _) -> i
  | Leaf(i) -> i

let rec max_id = function
  | Node(i, tL) -> List.fold_left max i (List.map max_id tL)
  | Leaf(i) -> i

let multifurcating_at_root = function
  | Node(_, tL) -> List.length tL > 2
  | Leaf(_) -> false

let rec plain_to_newick = function
  | Node(i, tL) ->
      "("^(String.concat "," (List.map plain_to_newick tL))^")"^(string_of_int i)
  | Leaf i -> string_of_int i

let rec ppr ff = function
  | Node(i, tL) ->
      Format.fprintf ff "@[(%a)@]%d"
        (Ppr.ppr_gen_list_inners "," ppr) tL i
  | Leaf i -> Format.pp_print_int ff i

(* the maximal outdegree of nodes in the tree *)
let rec outdegree = function
  | Node(_,tL) ->
      List.fold_left max (List.length tL) (List.map outdegree tL)
  | Leaf _ -> 0

(* increase all of the indices of the tree by "by" *)
let rec boost by = function
  | Node(i,tL) -> Node(i+by, List.map (boost by) tL)
  | Leaf(i) -> Leaf(i+by)

let recur f_node f_leaf tree =
  let rec aux = function
  | Node(id, tL) -> f_node id (List.map aux tL)
  | Leaf id -> f_leaf id
  in
  aux tree

(* for functions that don't treat leaves differently than a node with empty
 * leaves. *)
let recur_listly f = recur f (fun id -> f id [])

let find target tree =
  let rec aux = function
    | (Leaf i as x) :: _
    | (Node (i, _) as x) :: _ when i = target -> x
    | Leaf _ :: rest -> aux rest
    | Node (_, subtrees) :: rest -> List.append subtrees rest |> aux
    | [] -> raise Not_found
  in
  aux [tree]

let parent_map t =
  let maybe_add accum i = function
    | Some p -> IntMap.add i p accum
    | None -> accum
  in
  let rec aux accum = function
    | (parent, Leaf i) :: rest ->
      aux
        (maybe_add accum i parent)
        rest
    | (parent, Node (i, tL)) :: rest ->
      aux
        (maybe_add accum i parent)
        (List.fold_left
           (fun accum t -> (Some i, t) :: accum)
           rest
           tL)
    | [] -> accum
  in
  aux IntMap.empty [None, t]

let reroot tree root =
  if top_id tree = root then tree else
  let rec aux = function
    | [] -> failwith "root not found (tried to reroot at leaf?)"
    | (Node (i, subtrees), path) :: _ when i = root ->
      (i, subtrees) :: path
    | (Leaf _, _) :: rest -> aux rest
    | (Node (i, subtrees), path) :: rest ->
      List.map
        (fun subtree ->
          let path' = (i, List.remove subtrees subtree) :: path in
          subtree, path')
        subtrees
      |> List.append rest
      |> aux
  in
  aux [tree, []]
    |> List.rev
    |> List.reduce (fun (bi, btl) (ai, atl) -> ai, node bi btl :: atl)
    |> uncurry node

let rec nodes_containing nodes = function
  | Leaf _ -> nodes
  | Node (i, subtrees) ->
    let nodes' = List.fold_left
      (nodes_containing nodes %> IntSet.union |> flip)
      IntSet.empty
      subtrees
    in
    if List.for_all (top_id %> flip IntSet.mem nodes') subtrees then
      IntSet.add i nodes'
    else
      nodes'

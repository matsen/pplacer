(* our tree data stucture with information.
 *
 * Gtree stands for general tree, meaning that we have a tree with any kind of
 * bark that has get/set methods for bl, node_label, and edge_label.
 *
 * We number all of the nodes and then have maps which tell us about the edges.
 * Note that the edge information is for the node directly above the node with
 * the chosen edge.
 * *)

open Ppatteries

(* these are raised when we are asked for something that isn't in a bark map *)
exception Lacking_bark of int
exception Lacking_branchlength of int
exception Lacking_node_label of int
exception Lacking_edge_label of int

exception Attachment_distal_bl_out_of_range of int * float

(* Rounding errors crop up when subtracting branch lengths.
 * This is our tolerance. *)
let bl_arithmetic_tol = 1e-10


type 'a gtree =
  {stree : Stree.stree;
  bark_map : 'a IntMap.t}

let gtree stree bark_map =
  {stree = stree; bark_map = bark_map}

let of_stree stree = {stree = stree; bark_map = IntMap.empty}

let get_stree t = t.stree
let get_bark_map t = t.bark_map
let get_bark t id =
  try IntMap.find id t.bark_map with
  | Not_found -> raise (Lacking_bark id)
let get_bark_opt t id =
  if IntMap.mem id t.bark_map then Some(get_bark t id) else None

let get_bl t id =
  try (get_bark t id)#get_bl with
  | Not_found -> raise (Lacking_branchlength id)
let get_node_label t id =
  try (get_bark t id)#get_node_label with
  | Not_found -> raise (Lacking_node_label id)
let get_edge_label t id =
  try (get_bark t id)#get_edge_label with
  | Not_found -> raise (Lacking_edge_label id)

let set_stree t stree = {t with stree = stree}
let set_bark_map t bark_map = {t with bark_map = bark_map}
let add_bark t id bark =
  set_bark_map t (IntMap.add id bark (get_bark_map t))

let set_bl t id bl =
  add_bark t id ((get_bark t id)#set_bl bl)

(* stree related *)
let top_id t = Stree.top_id (get_stree t)
let n_taxa t = Stree.n_taxa (get_stree t) (* Number of non-root edges. *)
let n_edges t = Stree.n_edges (get_stree t) (* Number of non-root edges. *)
let recur f_node f_leaf t = Stree.recur f_node f_leaf (get_stree t)
let recur_listly f t = Stree.recur_listly f (get_stree t)
let node_ids t = Stree.node_ids (get_stree t)
let nonroot_node_ids t = Stree.nonroot_node_ids (get_stree t)
let leaf_ids t = Stree.leaf_ids (get_stree t)

(* bark map related *)
let add_bark id b t =
  { t with bark_map = (IntMap.add id b (get_bark_map t)) }
let map_bark_map f t = {t with bark_map = IntMap.map f (get_bark_map t)}
let mapi_bark_map f t = {t with bark_map = IntMap.mapi f (get_bark_map t)}

let fold_over_leaves f t v =
  let open Stree in
  let rec aux v = function
    | (Leaf i) :: rest when IntMap.mem i t.bark_map ->
      aux (f i (IntMap.find i t.bark_map) v) rest
    | Leaf _ :: rest -> aux v rest
    | Node (_, subtrees) :: rest -> aux v (List.append subtrees rest)
    | [] -> v
  in
  aux v [t.stree]

let leaf_bark_map t = fold_over_leaves IntMap.add t IntMap.empty

(* general *)

let compare bark_compare t1 t2 =
  try
    raise_if_different compare (get_stree t1) (get_stree t2);
    raise_if_different
      (IntMap.compare bark_compare)
      (get_bark_map t1)
      (get_bark_map t2);
    0
  with
  | Different c -> c

let tree_length tree =
  let get_our_bl id = get_bl tree id in
  let rec aux = function
    | Stree.Node(id, tL) ->
        List.fold_left
          ( +. )
          (get_our_bl id)
          (List.map aux tL)
    | Stree.Leaf id -> get_our_bl id
  in
  match get_stree tree with
  | Stree.Node(_, tL) ->
      (* exclude the root edge *)
      List.fold_left ( +. ) 0. (List.map aux tL)
  | Stree.Leaf _ -> 0.


(* adding things to trees *)

type 'a addition =
  | Subtree of 'a gtree
  | Internal_node

let addition_n_edges = function
  | Subtree t -> n_edges t
  | Internal_node -> 0

(* copy the info from src at id over to dest *)
let copy_bark ~dest ~src id =
  match IntMap.Exceptionless.find id (get_bark_map src) with
    | Some bark -> add_bark id bark dest
    | None -> dest

(* swap the bark for the two given ids *)
let swap_bark a b ({bark_map = m} as t) =
  let av, m' = IntMap.opt_extract a m in
  let bv, m'' = IntMap.opt_extract b m' in
  {t with bark_map = IntMap.opt_add b av m'' |> IntMap.opt_add a bv}

(* join a list of info_trees *)
let join new_id tL =
  gtree
    (Stree.node new_id (List.map get_stree tL))
    (List.reduce
      Bark_map.union
      (List.map get_bark_map tL))

let boost by = function
  | Subtree t ->
    Subtree
      (gtree
        (Stree.boost by (get_stree t))
        (Bark_map.boost by (get_bark_map t)))
  | Internal_node -> Internal_node

(* join two trees and add bark to the top *)
let opt_join t t_opt new_id bark =
  add_bark
    new_id
    bark
    (join
      new_id
      (match t_opt with
      | Subtree t_new -> [t; t_new]
      | Internal_node -> [t]))


(* add a subtree above the prev one, giving
 *
 *                     |
 *                     | - - new_t
 * root bl = orig bl   |
 *                     | bl = where
 *                     |
 *                     t
 *
 * we boost all of the indices in new_t by boost_by, and take the newly created
 * node to have id = 1 + top_id of boosted new_t.
 * bark_of_bl is called with the new top bl to equip the root of the new tree
 * with bark corresponding to the branch length bl.
 *)
let add_boosted_subtree_above bark_of_bl ~t ~new_t where boost_by =
  let our_top_id = top_id t in
  let boosted_new_t = boost boost_by new_t in
  let new_id = 1 + boost_by + (addition_n_edges new_t) in
  let final_top_bl =
    let new_top_bl = (get_bl t our_top_id) -. where in
    if new_top_bl >= 0. then new_top_bl
    else begin
      (* new_top_bl is neg then highest_distal was bigger than top edge *)
      if abs_float new_top_bl < bl_arithmetic_tol then
        0. (* ignore it if it's within tolerance *)
      else
        raise (Attachment_distal_bl_out_of_range (our_top_id, new_top_bl))
    end
  in
  opt_join
    (set_bl t our_top_id where)
    boosted_new_t
    new_id
    (bark_of_bl final_top_bl)

(* add a collection of subtrees given a list of (where, tree) pairs; where
 * describes where the tree should be glued in. pos is the "where" of the
 * previous lowest tree.
 * we assume that the top_id for each of the subtrees is equal to the number of
 * internal nodes of new_t minus one, as it would be from a postorder traversal,
 * starting from zero.
 * we assume that all ids >= avail_id are available.
 *)
let add_subtrees_above avail_id tree where_subtree_list =
  let rec aux pos accu accu_id = function
    | [] -> accu_id, accu
    | (new_pos, new_t, bark_of_bl)::rest ->
        aux
          new_pos
          (add_boosted_subtree_above
            bark_of_bl
            ~t:accu
            ~new_t
            (new_pos -. pos)
            accu_id)
          (2 + accu_id + (addition_n_edges new_t))
          rest
  in
  let get_where (where,_,_) = where in
  aux
    0.
    tree
    avail_id
    (List.sort (comparing get_where) where_subtree_list)

(* where_subtree_map is a map from a location to a triple (pos, tree,
 * bark_of_bl), where pos is the position along the edge in terms of distal
 * branch length, tree is the tree to attach, and bark_of_bl is the function to
 * use when making a new bark out of the branch length which has been cut down.
 * we assume that all input trees have their maximal ids at the top. *)
let add_subtrees_by_map ref_tree where_subtree_map =
  (* here we keep track of the available ids so that we can use new ones *)
  let global_avail_id = ref (1+(top_id ref_tree)) in
  let globalized_id_add_subtrees_above tree where_subtree_list =
    let new_id, result =
      add_subtrees_above !global_avail_id tree where_subtree_list in
    global_avail_id := new_id;
    result
  in
  (* add the info from ref tree back in and then add above subtrees *)
  let our_add_above below =
    let id = top_id below in
    try
      globalized_id_add_subtrees_above
        (copy_bark ~src:ref_tree ~dest:below id)
        (if IntMap.mem id where_subtree_map then
          IntMap.find id where_subtree_map
        else [])
    with
    | Attachment_distal_bl_out_of_range(_, new_top_bl) ->
        failwith
          (Printf.sprintf
            "Internal edge became %g when attaching subtrees to %d"
            new_top_bl id)
  in
  (* fill the stree skeleton back in with info and add subtrees *)
  let rec aux = function
    | Stree.Node(i, tL) -> our_add_above (join i (List.map aux tL))
    | Stree.Leaf i -> our_add_above (of_stree (Stree.Leaf i))
  in
  aux (get_stree ref_tree)


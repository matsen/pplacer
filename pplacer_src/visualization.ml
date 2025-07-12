type tree_fmt = Newick | Phyloxml

open Ppatteries

let intmap_of_arr a =
  let m = ref IntMap.empty in
  Array.iteri (fun i x -> m := IntMap.add i x (!m)) a;
  !m

(* writing various tree formats *)
let trees_to_file ?(with_suffix = true) tree_fmt prefix trees =
  let fname = match tree_fmt with
    | _ when not with_suffix -> prefix
    | Newick -> prefix ^ ".tre"
    | Phyloxml -> prefix ^ ".xml"
  in
  match tree_fmt with
    | Newick -> Newick_gtree.tree_list_to_file trees fname
    | Phyloxml -> Phyloxml.gtrees_to_file fname trees

let make_zero_leaf decor_list bl node_label =
  Gtree.Subtree
    (Gtree.gtree
      (Stree.leaf 0)
      (IntMap.add
        0
        (new Decor_bark.decor_bark
          (`Of_bl_node_edge_label_decor
            (Some bl, Some node_label, None, decor_list)))
        IntMap.empty))

let decor_bark_of_bl bl =
  new Decor_bark.decor_bark
    (`Of_bl_node_edge_label_decor (Some bl, None, None, []))

(* given a function that takes a location and a list of somethings and returns a
 * (where, tree) list for that location, make a tree containing those extra
 * subtrees given a something map
 *)
let tree_by_map f ref_tree placed_map =
  Gtree.add_subtrees_by_map
    ref_tree
    (IntMap.mapi f placed_map)

(* num tree *)
let num_tree bogus_bl ref_tree placed_map =
  tree_by_map
    (fun loc pqueries ->
      [((Gtree.get_bl ref_tree loc) /. 2.,
      make_zero_leaf
        [ Decor.red ]
        bogus_bl
        (Printf.sprintf "%d_at_%d" (List.length pqueries) loc),
      decor_bark_of_bl)])
    ref_tree
    placed_map

(* fat trees.
 * massm are By_edge mass maps.
 * *)

(* make edges which have a nonzero width at least a certain length *)
let spread_short_fat min_bl t =
  Gtree.set_bark_map t
    (IntMap.map
      (fun b ->
        try
          List.iter
            (function (Decor.Width _) -> raise Exit | _ -> ()) b#get_decor; b
        with
        | Exit -> begin
            (* it has some width *)
            match b#get_bl_opt with
            | None -> b#set_bl min_bl
            | Some bl -> if bl > min_bl then b else b#set_bl min_bl
          end)
      (Gtree.get_bark_map t))

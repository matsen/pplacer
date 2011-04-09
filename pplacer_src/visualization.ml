type tree_fmt = Newick | Phyloxml

open Subcommand
open MapsSets
open Fam_batteries

let min_width = 1.

(* log_coeff determines if we should apply a log transformation. we return a
 * list, which is empty if the final width is less than min_width *)
let widthl_of_mass log_coeff mass_width mass =
  let final_width =
    if log_coeff <> 0. then mass_width *. (log (1. +. log_coeff *. mass))
    else mass_width *. mass
  in
  if final_width >= min_width then [Decor.width (mass_width *. mass)] else []

(* writing various tree formats *)
let trees_to_file tree_fmt prefix trees =
  match tree_fmt with
  | Newick -> Newick_gtree.tree_list_to_file trees (prefix^".tre")
  | Phyloxml ->
    let pd = Phyloxml.pxdata_of_gtrees trees in
    Phyloxml.pxdata_to_file (prefix^".xml") pd

let make_zero_leaf decor_list bl name =
  Gtree.Subtree
    (Gtree.gtree
      (Stree.leaf 0)
      (IntMap.add
        0
        (new Decor_bark.decor_bark
          (`Of_bl_name_boot_dlist
            (Some bl, Some name, None, decor_list)))
        IntMap.empty))

let decor_bark_of_bl bl =
  new Decor_bark.decor_bark
    (`Of_bl_name_boot_dlist (Some bl, None, None, []))

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

let write_num_file bogus_bl tree_fmt fname_base ref_tree
                                                   placed_map =
  trees_to_file
    tree_fmt
    (fname_base^".num")
    [num_tree bogus_bl ref_tree placed_map]


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

let fat_tree ?min_bl mass_width log_coeff decor_ref_tree massm =
  let t =
    Decor_gtree.add_decor_by_map
      decor_ref_tree
      (IntMap.map
        (fun m ->
          [ Decor.sand ] @
          (widthl_of_mass log_coeff mass_width m))
        massm)
  in
  match min_bl with Some mb -> spread_short_fat mb t | None -> t

(* min_bl is the bl that will be fed to spread_short_fat above *)
let write_fat_tree
      ?min_bl mass_width log_coeff fname_base decor_ref_tree massm =
  Phyloxml.named_gtree_to_file
    (fname_base ^ ".fat.xml")
    (fname_base ^ ".fat")
    (fat_tree ?min_bl mass_width log_coeff decor_ref_tree massm)


class viz_command () =
object
  val unit_width = flag "--unit-width"
    (Plain (0., "Set the number of pixels for a single placement (will override total-width if set)."))
  val total_width = flag "--total-width"
    (Formatted (400., "Set the total number of pixels for all of the mass. Default: %g"))
  val xml = flag "--xml"
    (Plain (false, "Write phyloXML (with colors) for all visualizations."))
  val show_node_numbers = flag "--node-numbers"
    (Plain (false, "Put the node numbers in where the bootstraps usually go."))

  method specl = [
    float_flag unit_width;
    float_flag total_width;
    toggle_flag xml;
    toggle_flag show_node_numbers;
  ]

  method private fmt = if fv xml then Phyloxml else Newick
  method private decor_ref_tree pr =
    let ref_tree = Placerun.get_ref_tree pr in
    Decor_gtree.of_newick_gtree
      (if not (fv show_node_numbers) then
          ref_tree
       else
          (Newick_gtree.make_boot_id ref_tree))
  method private mass_width n_placed =
    let unit_width = fv unit_width in
    if unit_width <> 0. then (* unit width specified *)
      unit_width *. (float_of_int n_placed)
    else (* split up the mass according to the number of queries *)
      (fv total_width)
end

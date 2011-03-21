open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

type tree_format =
  | Phyloxml
  | Newick

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

let sing_tree weighting criterion mass_width ref_tree pquery =
  let pqname = String.concat "_" pquery.Pquery.namel in
  match weighting with
  | Mass_map.Weighted ->
    Gtree.add_subtrees_by_map
      ref_tree
      (IntMapFuns.of_pairlist_listly
        (ListFuns.mapi
          (fun num p ->
            let mass = criterion p in
            (Placement.location p,
              (Placement.distal_bl p,
              make_zero_leaf
                ([ Decor.red] @
                  (widthl_of_mass 0. mass_width mass))
                (Placement.pendant_bl p)
                (Printf.sprintf
                  "%s_#%d_M=%g"
                  pqname
                  num
                  mass),
              decor_bark_of_bl)))
          (Pquery.place_list pquery)))
  | Mass_map.Unweighted ->
      let p = Pquery.best_place criterion pquery in
      Gtree.add_subtrees_by_map
        ref_tree
        (IntMapFuns.of_pairlist_listly
          [Placement.location p,
            (Placement.distal_bl p,
            make_zero_leaf
              [ Decor.red; ]
              (Placement.pendant_bl p)
              (Printf.sprintf "%s" pqname),
              decor_bark_of_bl)])

let write_sing_file weighting criterion mass_width tree_fmt fname_base ref_tree
    placed_pquery_list =
  trees_to_file
    tree_fmt
    (fname_base^".sing")
    (List.map
      (sing_tree weighting criterion mass_width ref_tree)
      placed_pquery_list)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile

  val unit_width = flag "--unit-width"
    (Plain (0., "Set the number of pixels for a single placement (will override total-width if set)."))
  val xml = flag "--xml"
    (Plain (false, "Write phyloXML (with colors) for all visualizations."))
  val show_node_numbers = flag "--node-numbers"
    (Plain (false, "Put the node numbers in where the bootstraps usually go."))

  method specl =
    super_mass#specl
    @ super_out_prefix#specl
    @ [
      float_flag unit_width;
      toggle_flag xml;
      toggle_flag show_node_numbers;
    ]

  method desc = ""
  method usage = "usage: sing [options] placefile[s]"

  method private placefile_action prl =
    let _, weighting, criterion = self#mass_opts in
    let unit_width = fv unit_width in
    let fmt = if fv xml then Phyloxml else Newick in
    List.iter
      (fun pr ->
        let ref_tree = Placerun.get_ref_tree pr in
        let decor_ref_tree =
          Decor_gtree.of_newick_gtree
            (if not (fv show_node_numbers) then
                ref_tree
             else
                (Newick_gtree.make_boot_id ref_tree))
        in
        let fname_base =
          (fv out_prefix) ^ (Placerun.get_name pr)
        in
        write_sing_file
          weighting
          criterion
          unit_width
          fmt
          fname_base
          decor_ref_tree
          (List.filter Pquery.is_placed (Placerun.get_pqueries pr))
      )
      prl
end

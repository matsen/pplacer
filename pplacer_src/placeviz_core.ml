(* the actual functionality of placeviz
*)

type tree_fmt = Newick | Phyloxml

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

(* tog tree *)
let tog_tree criterion ref_tree placed_map =
  tree_by_map
    (fun _ ->
      List.map
        (fun pquery ->
          let best = Pquery.best_place criterion pquery in
          (Placement.distal_bl best,
          make_zero_leaf
            [ Decor.red ]
            (Placement.pendant_bl best)
            (String.concat "_" pquery.Pquery.namel),
         decor_bark_of_bl)))
    ref_tree
    placed_map

let write_tog_file tree_fmt criterion fname_base ref_tree placed_map =
  trees_to_file
    tree_fmt
    (fname_base^".tog")
    [tog_tree criterion ref_tree placed_map]

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

(* sing trees *)
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
  let pd = Phyloxml.pxdata_of_named_gtree
    (fname_base ^ ".fat")
    (fat_tree ?min_bl mass_width log_coeff decor_ref_tree massm)
  in Phyloxml.pxdata_to_file (fname_base ^ ".fat.xml") pd

(* edpl trees *)
let edpl_tree white_bg
      weighting criterion ~mass_width log_coeff max_edpl decor_ref_tree pr =
  let gray = if white_bg then Decor.black else Decor.white in
  Decor_gtree.add_decor_by_map
    decor_ref_tree
    (IntMap.map
      (fun (mass, edpl) ->
        (widthl_of_mass log_coeff mass_width mass) @
          [if edpl <= max_edpl then
            Decor.color_avg (edpl /. max_edpl) Decor.red gray
          else
            Decor.orange ])
      (Edpl.weighted_edpl_map_of_pr weighting criterion pr))

let write_edpl_tree white_bg weighting criterion ~mass_width log_coeff max_edpl fname_base decor_ref_tree placerun =
  let pd = Phyloxml.pxdata_of_named_gtree
    (fname_base ^ ".epdl")
    (edpl_tree white_bg weighting criterion ~mass_width log_coeff max_edpl decor_ref_tree placerun)
  in Phyloxml.pxdata_to_file (fname_base ^ ".epdl.xml") pd

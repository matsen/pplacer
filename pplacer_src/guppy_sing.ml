open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

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
              Visualization.make_zero_leaf
                [
                  Decor.red;
                  Decor.width (mass_width *. mass);
                ]
                (Placement.pendant_bl p)
                (Printf.sprintf
                  "%s_#%d_M=%g"
                  pqname
                  num
                  mass),
              Visualization.decor_bark_of_bl)))
          (Pquery.place_list pquery)))
  | Mass_map.Unweighted ->
      let p = Pquery.best_place criterion pquery in
      Gtree.add_subtrees_by_map
        ref_tree
        (IntMapFuns.of_pairlist_listly
          [Placement.location p,
            (Placement.distal_bl p,
            Visualization.make_zero_leaf
              [ Decor.red; ]
              (Placement.pendant_bl p)
              (Printf.sprintf "%s" pqname),
              Visualization.decor_bark_of_bl)])

let write_sing_file weighting criterion mass_width tree_fmt fname_base ref_tree
    placed_pquery_list =
  Visualization.trees_to_file
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
  inherit fat_cmd () as super_fat
  inherit classic_viz_cmd () as super_classic_viz

  method specl =
    super_mass#specl
    @ super_out_prefix#specl
    @ super_fat#specl

  method desc = "makes one tree for each query sequence, showing uncertainty"
  method usage = "usage: sing [options] placefile[s]"

  method private placefile_action prl =
    let _, weighting, criterion = self#mass_opts in
    let mass_width = self#multiplier_of_abs_tot 1. in
    List.iter
      (fun pr ->
        let fname_base =
          (fv out_prefix) ^ (Placerun.get_name pr)
        in
        write_sing_file
          weighting
          criterion
          mass_width
          self#fmt
          fname_base
          (self#decor_ref_tree pr)
          (List.filter Pquery.is_placed (Placerun.get_pqueries pr)))
      prl
end

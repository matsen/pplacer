open Subcommand
open Guppy_cmdobjs
open Ppatteries

let sing_tree weighting criterion mass_width ref_tree pquery =
  let pqname = String.concat "_" (Pquery.namel pquery) in
  match weighting with
  | Mass_map.Spread ->
    Gtree.add_subtrees_by_map
      ref_tree
      (IntMap.of_pairlist_listly
        (List.mapi
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
  | Mass_map.Point ->
      let p = Pquery.best_place criterion pquery in
      Gtree.add_subtrees_by_map
        ref_tree
        (IntMap.of_pairlist_listly
          [Placement.location p,
            (Placement.distal_bl p,
            Visualization.make_zero_leaf
              [ Decor.red; ]
              (Placement.pendant_bl p)
              (Printf.sprintf "%s" pqname),
              Visualization.decor_bark_of_bl)])

let sing_trees weighting criterion mass_width ref_tree placed_pquery_list =
  List.map
    (sing_tree weighting criterion mass_width ref_tree)
    placed_pquery_list

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit fat_cmd () as super_fat
  inherit! classic_viz_cmd () as super_classic_viz

  method specl =
    super_mass#specl
    @ super_output#specl
    @ super_fat#specl

  method desc = "makes one tree for each query sequence, showing uncertainty"
  method usage = "usage: sing [options] placefile[s]"

  method private placefile_action prl =
    let weighting, criterion = self#mass_opts in
    let mass_width = self#multiplier_of_abs_tot 1. in
    let trees = List.map
      (fun pr ->
        Placerun.get_name pr,
        sing_trees
          weighting
          criterion
          mass_width
          (self#decor_ref_tree pr)
          (List.filter Pquery.is_placed (Placerun.get_pqueries pr)))
      prl
    in
    self#write_trees ".sing" trees (self#out_file_or_dir ())

end

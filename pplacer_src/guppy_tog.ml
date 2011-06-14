open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries
open Visualization

(* tog tree *)
let tog_tree criterion ref_tree placed_map =
  tree_by_map
    (fun _ ->
      List.map
        (fun pquery ->
          let best = Pquery.best_place criterion pquery in
          let n_names = List.length (Pquery.namel pquery) in
          let addition =
            if n_names = 1 then
              make_zero_leaf
                [ Decor.red ]
                (Placement.pendant_bl best)
                (String.concat "_" (Pquery.namel pquery))
            else
              let tree =
                Stree.node
                  n_names
                  (List.map Stree.leaf (Base.range n_names))
              in
              let decor_map = IntMap.add
                n_names
                (new Decor_bark.decor_bark
                   (`Of_bl_name_boot_dlist
                       (Some (Placement.pendant_bl best),
                        None,
                        None,
                        [Decor.red])))
                IntMap.empty
              in
              let decor_map = List.fold_left2
                (fun accum i name ->
                  IntMap.add
                    i
                    (new Decor_bark.decor_bark
                       (`Of_bl_name_boot_dlist
                           (Some 0.0,
                            Some name,
                            None,
                            [Decor.red])))
                    accum)
                decor_map
                (Base.range n_names)
                (Pquery.namel pquery)
              in
              Gtree.Subtree (Gtree.gtree tree decor_map)
          in
          Placement.distal_bl best,
          addition,
          decor_bark_of_bl))
    ref_tree
    placed_map

let write_tog_file tree_fmt criterion fname_base ref_tree placed_map =
  trees_to_file
    tree_fmt
    (fname_base^".tog")
    [tog_tree criterion ref_tree placed_map]

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit classic_viz_cmd () as super_classic_viz

  method specl =
    super_mass#specl
    @ super_output#specl
    @ super_classic_viz#specl

  method desc = "makes a tree with each of the reads represented as a pendant edge"
  method usage = "usage: tog [options] placefile[s]"

  method private placefile_action prl =
    let _, _, criterion = self#mass_opts in
    let trees = List.map
      (fun pr ->
        let _, placed_map =
          Pquery.make_map_by_best_loc
            criterion
            (Placerun.get_pqueries pr)
        in
        Placerun.get_name pr,
        [
          tog_tree
            criterion
            (self#decor_ref_tree pr)
            placed_map
        ])
      prl
    in
    self#write_trees ".tog" trees (self#out_file_or_dir ())
end

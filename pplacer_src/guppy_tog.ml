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

class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit viz_command () as super_viz

  method specl =
    super_mass#specl
    @ super_out_prefix#specl
    @ super_viz#specl

  method desc = "make a tree with each of the fragments represented as a pendant edge"
  method usage = "usage: tog [options] placefile[s]"

  method private placefile_action prl =
    let _, _, criterion = self#mass_opts in
    List.iter
      (fun pr ->
        let _, placed_map =
          Pquery.make_map_by_best_loc
            criterion
            (Placerun.get_pqueries pr)
        in
        let fname_base =
          (fv out_prefix) ^ (Placerun.get_name pr)
        in
        write_tog_file
          self#fmt
          criterion
          fname_base
          (self#decor_ref_tree pr)
          placed_map)
      prl
end

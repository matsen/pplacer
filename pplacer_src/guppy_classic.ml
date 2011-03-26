open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries
open Visualization


class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit viz_command () as super_viz

  val log_coeff = flag "--log"
    (Plain (0., "Set to a nonzero value to perform a logarithmic transform of the branch width."))
  val bogus_bl = flag "--num-bl"
    (Formatted (0.1, "Set the branch length for visualization in the num tree. Default: %g"))
  val min_fat_bl = flag "--min-fat"
    (Formatted (1e-2, "The minimum branch length for fattened edges (to increase their visibility). To disable, specify a value of 0. Default: %g"))

  method specl =
    super_mass#specl
    @ super_out_prefix#specl
    @ super_viz#specl
    @ [
      float_flag log_coeff;
      float_flag bogus_bl;
      float_flag min_fat_bl;
    ]

  method desc = "writes fat and num trees in their individual files"
  method usage = "usage: classic [options] placefile[s]"

  method private placefile_action prl =
    let transform, weighting, criterion = self#mass_opts in
    List.iter
      (fun pr ->
        let decor_ref_tree = self#decor_ref_tree pr
        and pqueries = Placerun.get_pqueries pr
        and min_bl = match fv min_fat_bl with
          | 0. -> None
          | x -> Some x
        and fname_base =
          (fv out_prefix) ^ (Placerun.get_name pr)
        in
        let place_massm =
          Mass_map.By_edge.of_placerun transform weighting criterion pr
        in
        let unplaced_seqs, placed_map =
          Pquery.make_map_by_best_loc
            criterion
            pqueries
        in
        let n_placed = (List.length pqueries) - (List.length unplaced_seqs) in
        begin
          write_num_file
            (fv bogus_bl)
            (self#fmt)
            fname_base
            decor_ref_tree
            placed_map;
          write_fat_tree
            ?min_bl
            (self#mass_width n_placed)
            (fv log_coeff)
            fname_base
            decor_ref_tree
            place_massm
        end)
      prl
end

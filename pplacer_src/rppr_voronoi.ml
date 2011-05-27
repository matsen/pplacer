open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  method specl =
    super_mass#specl
    @ super_refpkg#specl
    @ super_output#specl

  method desc = "apply voronoi"
  method usage = "usage: voronoi [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let transform, weighting, criterion = self#mass_opts in
      let mass = Mass_map.Indiv.of_placerun transform weighting criterion pr
      and graph = Voronoi.of_gtree (Placerun.get_ref_tree pr) in
      let mass_dist = Voronoi.distribute_mass graph mass in
      IntMap.iter
        (fun e fl ->
          Printf.printf "%d " e;
          List.iter (Printf.printf "%0.6f ") fl;
          print_newline ())
        mass_dist;

    | _ -> failwith "voronoi takes exactly one placefile"

end

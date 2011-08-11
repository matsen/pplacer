open Subcommand
open Guppy_cmdobjs
open Ppatteries

module I = Mass_map.Indiv

let merge a x = a := x +. !a
let lmerge = List.fold_left ((!) |- (+.) |> flip) 0. |- ref

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  method specl =
    super_mass#specl
    @ super_output#specl

  method desc =
"calculate weighted phylogenetic diversity of placefiles"
  method usage = "usage: wpd [options] placefile[s]"

  method private placefile_action prl =
    let transform, weighting, criterion = self#mass_opts in
    let indiv_of = I.of_placerun transform weighting criterion in
    List.iter
      (fun pr ->
        let gt = Placerun.get_ref_tree pr
        and mass = indiv_of pr in
        let partial_total id = Kr_distance.total_along_edge
          (fun r -> 2. *. (min !r (1. -. !r)))
          (Gtree.get_bl gt id)
          (IntMap.get id [] mass |> List.map I.to_pair |> List.sort)
          merge
        in
        Kr_distance.total_over_tree
          partial_total
          (const ())
          lmerge
          (fun () -> ref 0.)
          gt
        |> Printf.printf "%s: %g\n" (Placerun.get_name pr))
      prl

end

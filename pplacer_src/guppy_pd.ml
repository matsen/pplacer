open Subcommand
open Guppy_cmdobjs
open Ppatteries

let merge a x = a := x +. !a
let lmerge = List.fold_left ((!) |- (+.) |> flip) 0. |- ref

module I = Mass_map.Indiv

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  val normalized = flag "--normalized"
    (Plain (false, "Divide by total tree length."))

  method specl =
    super_mass#specl
  @ super_output#specl
  @ [toggle_flag normalized]

  method desc = "calculate phylogenetic diversity"
  method usage = "usage: pd [options] placefile[s]"

  method private placefile_action prl =
    let transform, weighting, criterion = self#mass_opts in
    let indiv_of = I.of_placerun transform weighting criterion in
    List.iter
      (fun pr ->
        let gt = Placerun.get_ref_tree pr
        and mass = indiv_of pr in
        let total_mass = I.total_mass mass in
        let partial_total id = Kr_distance.total_along_edge
          (* When we're passing along the induced tree, the mass will be on the
           * range (0, total_mass), and the branch length multiplier should be
           * 1. Otherwise, we're either before or after the induced tree and the
           * multiplier should be 0. *)
          (fun r -> if !r = 0. || approx_equal !r total_mass then 0. else 1.)
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
        |> (if not (fv normalized) then identity
          else fun pd -> pd /. (Gtree.tree_length gt))
        |> Printf.printf "%s: %g\n" (Placerun.get_name pr))
      prl

end

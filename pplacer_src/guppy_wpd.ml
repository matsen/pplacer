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
        Guppy_pd.total_along_mass
          (Placerun.get_ref_tree pr)
          (indiv_of pr)
          (fun r -> 1. -. 2. *. !r)
        |> Printf.printf "%s: %g\n" (Placerun.get_name pr))
      prl

end

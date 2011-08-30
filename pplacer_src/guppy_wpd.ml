open Subcommand
open Guppy_cmdobjs
open Ppatteries

let wpd_of_placerun criterion pr =
  Guppy_pd.total_along_mass
    (Placerun.get_ref_tree pr)
    (Mass_map.Indiv.of_placerun
       Mass_map.Unweighted
       criterion
       pr)
    (fun r -> 2. *. (min !r (1. -. !r)))

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  method specl =
    super_mass#specl
    @ super_tabular#specl

  method desc =
"calculate weighted phylogenetic diversity of placefiles"
  method usage = "usage: wpd [options] placefile[s]"

  method private placefile_action prl =
    let _, criterion = self#mass_opts in
    let wpd = wpd_of_placerun criterion in
    prl
      |> List.map
          (fun pr -> [Placerun.get_name pr; wpd pr |> Printf.sprintf "%g"])
      |> List.cons ["name"; "wpd"]
      |> self#write_ll_tab

end

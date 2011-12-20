open Subcommand
open Guppy_cmdobjs
open Ppatteries

let pentropy_of_placerun criterion pr =
  let mass_map = Mass_map.Indiv.of_placerun Mass_map.Point criterion pr in
  Guppy_pd.total_along_mass
    (Placerun.get_ref_tree pr)
    mass_map
    (fun r bl -> if approx_equal !r 0. then 0. else bl *. !r *. (log !r))
  |> (~-.)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~point_choice_allowed:false () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  method specl =
    super_mass#specl
    @ super_tabular#specl

  method desc =
"calculate weighted phylogenetic entropy of placefiles"
  method usage = "usage: pentropy [options] placefile[s]"

  method private placefile_action prl =
    let criterion = self#criterion in
    let pe = pentropy_of_placerun criterion in
    prl
      |> List.map
          (fun pr -> [Placerun.get_name pr; pe pr |> Printf.sprintf "%g"])
      |> self#write_ll_tab

end

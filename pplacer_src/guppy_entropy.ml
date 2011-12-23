open Subcommand
open Guppy_cmdobjs
open Ppatteries

let entropy_of_placerun criterion pr =
  let total = Guppy_pd.total_along_mass
    (Placerun.get_ref_tree pr)
    (Mass_map.Indiv.of_placerun Mass_map.Point criterion pr)
  in
  let phylo r bl = if approx_equal !r 0. then 0. else bl *. !r *. (log !r)
  and quadro r bl = bl *. !r *. (1. -. !r) in
  ~-. (total phylo), total quadro

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
"calculate weighted phylogenetic and quadratic entropy of placefiles"
  method usage = "usage: entropy [options] placefile[s]"

  method private placefile_action prl =
    let criterion = self#criterion in
    prl
      |> List.map
          (fun pr ->
            let pe, qe = entropy_of_placerun criterion pr in
            Placerun.get_name pr
            :: List.map (Printf.sprintf "%g") [pe; qe])
      |> List.cons ["placerun"; "phylogenetic"; "quadratic"]
      |> self#write_ll_tab

end

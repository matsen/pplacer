open Subcommand
open Guppy_cmdobjs
open Ppatteries

let reflect x =
  if approx_equal ~epsilon:Guppy_pd.epsilon x 1. then 0.
  else begin
    let y = 1. -. x in
    if y < 0. then failwith "reflect out of range!"
    else y
  end

let wpd_of_placerun exponent criterion pr =
  let mass_map = Mass_map.Indiv.of_placerun Mass_map.Point criterion pr in
  let f =
    if exponent < 0. || exponent > 1. then
      failwith("exponent must be between 0 and 1, inclusive")
    else if exponent = 1. then (fun x -> 2. *. x)
    else if exponent = 0. then Guppy_pd.bump_function
    else (fun x -> (2. *. x) ** exponent)
  in
  Guppy_pd.total_along_mass
    (Placerun.get_ref_tree pr)
    mass_map
    (fun r bl -> min (f !r) (f (reflect !r)) *. bl)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~point_choice_allowed:false () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  val exponent = flag "--exponent"
    (Formatted (1., "An exponent for WPD calculation. Default: %g."))

  method specl =
    super_mass#specl
    @ super_tabular#specl
    @ [
      float_flag exponent;
    ]

  method desc =
"calculate weighted phylogenetic diversity of placefiles"
  method usage = "usage: wpd [options] placefile[s]"

  method private placefile_action prl =
    let criterion = self#criterion in
    let wpd = wpd_of_placerun (fv exponent) criterion in
    prl
      |> List.map
          (fun pr -> [Placerun.get_name pr; wpd pr |> Printf.sprintf "%g"])
      |> self#write_ll_tab

end

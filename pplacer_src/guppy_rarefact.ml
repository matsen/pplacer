open Subcommand
open Guppy_cmdobjs
open Ppatteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  method specl =
    super_mass#specl
  @ super_tabular#specl

  method desc = "calculate phylogenetic rarefaction"
  method usage = "usage: rarefact [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let transform, weighting, criterion = self#mass_opts in
      let indiv_of = Mass_map.Indiv.of_placerun transform weighting criterion in
      Rarefaction.of_placerun indiv_of pr
        |> Enum.map (fun (a, b) -> [string_of_int a; Printf.sprintf "%g" b])
        |> List.of_enum
        |> List.cons ["k"; "r"]
        |> self#write_ll_tab

    | _ -> failwith "rarefact takes exactly one placefile"

end

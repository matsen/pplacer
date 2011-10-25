open Subcommand
open Guppy_cmdobjs
open Ppatteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~weighting_allowed:false () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  method specl =
    super_mass#specl
  @ super_tabular#specl

  method desc = "calculates phylogenetic rarefaction curves"
  method usage = "usage: rarefact [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let criterion = self#criterion in
      Rarefaction.of_placerun criterion pr
        |> Enum.map (fun (a, b) -> [string_of_int a; Printf.sprintf "%g" b])
        |> List.of_enum
        |> List.cons ["k"; "r"]
        |> self#write_ll_tab

    | l ->
      List.length l
      |> Printf.sprintf "rarefact takes exactly one placefile (%d given)"
      |> failwith

end

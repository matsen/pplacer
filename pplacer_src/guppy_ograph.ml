open Subcommand
open Guppy_cmdobjs
open Ppatteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output
  inherit mass_cmd ~point_choice_allowed:false () as super_mass

  method specl =
    super_output#specl
  @ super_mass#specl

  method desc = "finds the mass islands of one or more pqueries"
  method usage = "usage: ograph [options] placefile[s]"

  method private placefile_action = function
    | [pr] ->
      Placerun.get_pqueries pr
      |> Mass_overlap.of_pql self#criterion
      |> Enum.map (String.join " ")
      |> File.write_lines (self#single_file ())

end

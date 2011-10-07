open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile

  val c = flag "-c"
    (Needs_argument ("c", "The c parameter for mass compression"))

  method specl =
    super_mass#specl
  @ super_output#specl
  @ [
    float_flag c;
  ]

  method desc = "compress a placefile's pqueries"
  method usage = "usage: compress [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let weighting, criterion = self#mass_opts in
      Mass_compress.of_placerun ~c:(fv c) weighting criterion pr
      |> Placerun.set_pqueries pr
      |> self#write_placefile "guppy compress" (self#single_file ())

    | l ->
      List.length l
      |> Printf.sprintf "compress takes exactly one placefile (%d given)"
      |> failwith

end

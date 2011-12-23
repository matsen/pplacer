open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile

  val cutoff = flag "--cutoff"
    (Needs_argument ("cutoff", "The cutoff parameter for mass compression"))

  method specl =
    super_mass#specl
  @ super_output#specl
  @ [
    float_flag cutoff;
  ]

  method desc = "compress a placefile's pqueries"
  method usage = "usage: compress [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let weighting, criterion = self#mass_opts in
      Mass_compress.of_placerun ~c:(fv cutoff) weighting criterion pr
      |> Placerun.set_pqueries pr
      |> self#write_placefile (self#single_file ())

    | l ->
      List.length l
      |> Printf.sprintf "compress takes exactly one placefile (%d given)"
      |> failwith

end

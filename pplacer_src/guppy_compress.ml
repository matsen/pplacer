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
  val discard_below = flag "--discard-below"
    (Plain (0., "In island clustering, ignore pquery locations with a mass less than the specified value."))
  val use_mcl = flag "--mcl"
    (Plain (false, "Use MCL clustering instead of island clustering."))

  method specl =
    super_mass#specl
  @ super_output#specl
  @ [
    float_flag cutoff;
    float_flag discard_below;
    toggle_flag use_mcl;
  ]

  method desc = "compresses a placefile's pqueries"
  method usage = "usage: compress [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let weighting, criterion = self#mass_opts in
      let cluster_fn =
        if fv use_mcl then Guppy_mcl.islands_of_pql else
          Mass_islands.of_pql ~discard_below:(fv discard_below)
      in
      Mass_compress.of_placerun
        ~c:(fv cutoff)
        cluster_fn
        weighting
        criterion
        pr
      |> Placerun.set_pqueries pr
      |> self#write_placefile (self#single_file ())

    | l ->
      List.length l
      |> Printf.sprintf "compress takes exactly one placefile (%d given)"
      |> failwith

end

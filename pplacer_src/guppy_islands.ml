open Subcommand
open Guppy_cmdobjs
open Ppatteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output
  inherit mass_cmd ~point_choice_allowed:false () as super_mass

  val discard_below = flag "--discard-below"
    (Plain (0., "Ignore pquery locations with a mass less than the specified value."))

  method specl =
    super_output#specl
  @ super_mass#specl
  @ [float_flag discard_below]

  method desc = "finds the mass islands of one or more pqueries"
  method usage = "usage: islands [options] placefile[s]"

  method private placefile_action prl =
    let gt = Mokaphy_common.list_get_same_tree prl
    and prefix = self#single_prefix ~requires_user_prefix:true () in
    List.map Placerun.get_pqueries prl
      |> List.flatten
      |> Mass_islands.of_pql
          ~discard_below:(fv discard_below)
          self#criterion
      |> List.iteri
          (fun e (_, pql) ->
            Placerun.make gt (string_of_int e) pql
              |> self#write_placefile
                  "guppy islands"
                  (Printf.sprintf "%s%d.jplace" prefix e))

end

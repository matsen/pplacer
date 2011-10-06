open Subcommand
open Guppy_cmdobjs
open Ppatteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  method desc = "find the mass islands of one or more pqueries"
  method usage = "usage: islands [options] placefile[s]"

  method private placefile_action prl =
    let gt = Mokaphy_common.list_get_same_tree prl
    and prefix = self#single_prefix ~requires_user_prefix:true () in
    List.map Placerun.get_pqueries prl
      |> List.flatten
      |> Mass_islands.of_pql
      |> List.iteri
          (fun e (edges, pql) ->
            IntSet.print ~first:"{" ~sep:", " ~last:"}\n" Int.print stdout edges;
            Placerun.make gt (string_of_int e) pql
              |> self#write_placefile
                  "guppy islands"
                  (Printf.sprintf "%s%d.jplace" prefix e))

end

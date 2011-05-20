open Subcommand
open Guppy_cmdobjs
open Placerun


class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  method desc =
"writes the number of leaves of the reference tree and the number of pqueries"
  method usage = "usage: info [options] placefile[s]"

  method private placefile_action prl =

    let ch = self#out_channel in

    Printf.fprintf ch "name\tleaves\tpqueries\n";

    List.iter
      (fun pr ->
        Printf.fprintf ch "%s\t%d\t%d\n"
          pr.name
          (Gtree.n_taxa pr.ref_tree)
          (n_pqueries pr))
      prl
end

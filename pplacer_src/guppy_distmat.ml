open Guppy_cmdobjs
open Subcommand

open Ppatteries

let write_dist_mat ff pr =
  Uptri.ppr_lowtri ff Edge_rdist.ppr_rdist
    (Edge_rdist.build_pairwise_dist (Placerun.get_ref_tree pr))

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output

  method desc =
"prints out a pairwise distance matrix between the edges"
  method usage = "usage: distmat -o my.tab placefile[s]"

  method action = function
    | pathl ->
       let ff = Format.formatter_of_out_channel self#out_channel in
       List.iter (write_dist_mat ff) (List.map Placerun_io.of_any_file pathl)
end

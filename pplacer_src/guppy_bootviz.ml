open Subcommand
open Guppy_cmdobjs
open Ppatteries

class cmd () =
object
  inherit subcommand () as super

  val boot_fname = flag "-b"
    (Needs_argument ("bootstrapped trees", "A single Newick file containing the bootstrapped trees, one per line."))
  val out_fname = flag "-o"
    (Formatted ("cluster_boot.xml", "Specify an out file. Default: %s"))
  val cutoff = flag "--cutoff"
    (Formatted (0., "Specify the cutoff for writing out the bootstrap value. Default: %g."))

  method specl = [
    string_flag boot_fname;
    string_flag out_fname;
    float_flag cutoff;
  ]

  method desc =
"makes a phyloXML tree showing the bootstrap values"
  method usage = "usage: bootviz [options] -b boot_trees cluster_tree"

  method action = function
    | [ct_fname] ->
      Phyloxml.gtree_to_file
        (fv out_fname)
        (Bootviz.decorate_tree
          (fv cutoff)
          (fv boot_fname)
          ct_fname)

    | l ->
      List.length l
      |> Printf.sprintf "bootviz takes exactly one cluster tree (%d given)"
      |> failwith

end

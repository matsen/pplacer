open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit rng_cmd () as super_rng

  val tree_size = flag "-s"
    (Needs_argument ("tree size", "The desired number of leaves in the tree."))
  val n_colors = flag "-c"
    (Needs_argument ("n_colors", "If specified, generate a colored tree with this many colors."))
  val node_prefix = flag "-p"
    (Formatted ("n", "The prefix for node names in non-colored trees. Default is \"%s\"."))

  method specl =
    super_rng#specl
  @ super_output#specl
  @ [
    int_flag tree_size;
    int_flag n_colors;
    string_flag node_prefix;
  ]

  method desc = "generate newick trees"
  method usage = "usage: gen_tree [options]"

  method action _ =
    let size = fv tree_size in
    let tree = match fvo n_colors with
      | Some n_colors ->
        Commiesim.random_colored_tree self#rng size n_colors
      | None ->
        Commiesim.gtree_of_stree_numbers
          (Commiesim.newick_bark_of_prefixed_int (fv node_prefix))
          (Commiesim.generate_yule self#rng size)
    in
    Newick_gtree.write self#out_channel tree

end

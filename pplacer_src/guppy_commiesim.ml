open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit rng_cmd () as super_rng
  inherit refpkg_cmd ~required:true as super_refpkg

  val n_select = flag "-n"
    (Needs_argument ("n_select", "The number of sets selected from the cut sets
    as we move down the tree."))
  val n_splits_mean = flag "-s"
    (Needs_argument ("n_splits_mean", "The mean number of splits chosen at an
    internal node."))
  val cluster_tree = flag "-t"
    (Needs_argument ("cluster_tree", "A file containing the clustering tree, in Newick format."))
  val n_pqueries = flag "-q"
    (Needs_argument ("n_pqueries", "The number of placements to put in each placefile."))

  method specl =
    super_refpkg#specl
  @ super_rng#specl
  @ super_output#specl
  @ [
    int_flag n_select;
    float_flag n_splits_mean;
    string_flag cluster_tree;
    int_flag n_pqueries;
  ]

  method desc = "generate placefiles"
  method usage = "usage: commiesim [options] -c my.refpkg"

  method action _ =
    let gt = Commiesim.main
      self#rng
      ~retries:100
      ~n_select:(fv n_select)
      ~n_splits_mean:(fv n_splits_mean)
      ~cluster_tree:(Newick_gtree.of_file (fv cluster_tree))
      ~n_pqueries:(fv n_pqueries)
      ~tree:(Refpkg.get_ref_tree self#get_rp)
      (self#single_prefix ())
    in
    Newick_gtree.to_file
      gt
      ((Filename.chop_suffix (fv cluster_tree) ".tre") ^ ".expand.tre")
end

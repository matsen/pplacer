open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd ~prefix_required:true () as super_output
  inherit rng_cmd () as super_rng
  inherit refpkg_cmd ~required:true as super_refpkg

  val include_prob = flag "-i"
    (Needs_argument ("include_prob", "The probability of including an individual leaf set. 0 means put all at root."))
  val poisson_mean = flag "-m"
    (Needs_argument ("poisson_mean", "The mean of the poisson distribution for number of splits to make"))
  val cluster_tree = flag "-t"
    (Needs_argument ("cluster_tree", "The clustering tree, in Newick format."))
  val n_pqueries = flag "-q"
    (Needs_argument ("n_pqueries", "The number of placements to put in each placefile."))

  method specl =
    super_refpkg#specl
  @ super_rng#specl
  @ super_output#specl
  @ [
    float_flag include_prob;
    float_flag poisson_mean;
    string_flag cluster_tree;
    int_flag n_pqueries;
  ]

  method desc = "generate placefiles"
  method usage = "usage: commiesim [options] -c my.refpkg"

  method action _ =
    let ip =
      match fv include_prob with
        | 0. -> None
        | x -> assert(x > 0.); Some x
    in
    Commiesim.main
      self#rng
      ?include_prob:ip
      ~retries:100
      ~poisson_mean:(fv poisson_mean)
      ~cluster_tree:(Newick_gtree.of_string (fv cluster_tree))
      ~n_pqueries:(fv n_pqueries)
      ~tree:(Refpkg.get_ref_tree self#get_rp)
      (self#single_prefix ~requires_user_prefix:true ())
end

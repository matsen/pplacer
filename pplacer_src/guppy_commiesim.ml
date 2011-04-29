open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit rng_cmd () as super_rng
  inherit refpkg_cmd ~required:true as super_refpkg

  val include_prob = flag "-i"
    (Needs_argument ("include_prob", "The probability of including an individual leaf set."))
  val poisson_mean = flag "-m"
    (Needs_argument ("poisson_mean", "The mean of the poisson distribution for number of splits to make"))
  val yule_size = flag "-s"
    (Needs_argument ("yule_size", "The number of leaves in the sample clustering tree."))
  val n_pqueries = flag "-q"
    (Needs_argument ("n_pqueries", "The number of placements to put in each placefile."))

  method specl =
    super_refpkg#specl
  @ super_rng#specl
  @ super_out_prefix#specl
  @ [
    float_flag include_prob;
    float_flag poisson_mean;
    int_flag yule_size;
    int_flag n_pqueries;
  ]

  method desc = "generate placefiles"
  method usage = "usage: commiesim [options] -c my.refpkg"

  method action _ =
    Splits.main
      self#rng
      (fv include_prob)
      (fv poisson_mean)
      (fv yule_size)
      (fv n_pqueries)
      (Refpkg.get_ref_tree self#get_rp)
      (fv out_prefix)
end

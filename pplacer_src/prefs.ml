(* preferences data type and functions.
 *)

open Ppatteries

type prefs =
  {
   (* basics *)
    refpkg_path : string ref;
    tree_fname : string ref;
    ref_align_fname : string ref;
    stats_fname : string ref;
    ref_dir : string ref;
    out_dir : string ref;
    (* tree calc *)
    start_pend : float ref;
    max_pend : float ref;
    initial_tolerance : float ref;
    calc_pp : bool ref;
    uniform_prior : bool ref;
    informative_prior : bool ref;
    prior_lower : float ref;
    pp_rel_err : float ref;
    (* playing ball *)
    fig_cutoff : float ref;
    evaluate_all : bool ref;
    evaluation_discrepancy: string ref;
    fig_tree : string ref;
    max_strikes : int ref;
    strike_box : float ref;
    max_pitches : int ref;
    fantasy : float ref;
    fantasy_frac : float ref;
    (* model *)
    emperical_freqs : bool ref;
    model_name : string ref;
    gamma_n_cat : int ref;
    gamma_alpha : float ref;
    (* reading and writing *)
    write_masked : bool ref;
    only_write_best : bool ref;
    (* other *)
    pretend : bool ref;
    check_like : bool ref;
    children : int ref;
    version : bool ref;
    timing : bool ref;
    no_pre_mask : bool ref;
    pre_masked_file : string ref;
    map_fasta : string ref;
    map_cutoff : float ref;
    map_identity : bool ref;
    keep_at_most : int ref;
    keep_factor : float ref;
    mrca_class : bool ref;
  }


(* default values *)
let defaults () =
  {
    (* basics *)
    refpkg_path = ref "";
    tree_fname = ref "";
    ref_align_fname = ref "";
    stats_fname = ref "";
    ref_dir = ref ""; (* empty is the correct default; it gets some special handling *)
    out_dir = ref ".";
    (* tree calc *)
    start_pend = ref 0.1;
    max_pend = ref 2.;
    initial_tolerance = ref 0.01;
    calc_pp = ref false;
    uniform_prior = ref false;
    informative_prior = ref false;
    prior_lower = ref 0.;
    pp_rel_err = ref 0.01;
    (* playing ball *)
    fig_cutoff = ref 0.;
    evaluate_all = ref false;
    evaluation_discrepancy = ref "";
    fig_tree = ref "";
    max_strikes = ref 6;
    strike_box = ref 3.;
    max_pitches = ref 40;
    fantasy = ref 0.;
    fantasy_frac = ref 0.1;
    (* model *)
    emperical_freqs = ref true;
    model_name = ref "LG";
    gamma_n_cat = ref 1;
    gamma_alpha = ref 1.;
    (* reading and writing *)
    write_masked = ref false;
    only_write_best = ref false;
    (* other *)
    pretend = ref false;
    check_like = ref false;
    children = ref 2;
    version = ref false;
    timing = ref false;
    no_pre_mask = ref false;
    pre_masked_file = ref "";
    map_fasta = ref "";
    map_cutoff = ref 0.8;
    map_identity = ref false;
    keep_at_most = ref 7;
    keep_factor = ref 0.01;
    mrca_class = ref false;
  }


type mut_param =
  | MutBool of bool ref
  | MutFloat of float ref
  | MutInt of int ref
  | MutString of string ref


(* yay visual block! *)
let refpkg_path       p = !(p.refpkg_path)
let tree_fname        p = !(p.tree_fname)
let ref_align_fname   p = !(p.ref_align_fname)
let stats_fname       p = !(p.stats_fname)
let start_pend        p = !(p.start_pend)
let max_pend          p = !(p.max_pend)
let initial_tolerance p = !(p.initial_tolerance)
let calc_pp           p = !(p.calc_pp)
let uniform_prior     p = !(p.uniform_prior)
let informative_prior p = !(p.informative_prior)
let prior_lower       p = !(p.prior_lower)
let pp_rel_err        p = !(p.pp_rel_err)
let fig_cutoff        p = !(p.fig_cutoff)
let evaluate_all      p = !(p.evaluate_all)
let evaluation_discrepancy p = !(p.evaluation_discrepancy)
let fig_tree          p = !(p.fig_tree)
let max_strikes       p = !(p.max_strikes)
let strike_box        p = !(p.strike_box)
let max_pitches       p = !(p.max_pitches)
let fantasy           p = !(p.fantasy)
let fantasy_frac      p = !(p.fantasy_frac)
let emperical_freqs   p = !(p.emperical_freqs)
let model_name        p = !(p.model_name)
let gamma_n_cat       p = !(p.gamma_n_cat)
let gamma_alpha       p = !(p.gamma_alpha)
let write_masked      p = !(p.write_masked)
let only_write_best   p = !(p.only_write_best)
let ref_dir           p = !(p.ref_dir)
let out_dir           p = !(p.out_dir)
let pretend           p = !(p.pretend)
let check_like        p = !(p.check_like)
let children          p = !(p.children)
let version           p = !(p.version)
let timing            p = !(p.timing)
let no_pre_mask       p = !(p.no_pre_mask)
let pre_masked_file   p = !(p.pre_masked_file)
let map_fasta         p = !(p.map_fasta)
let map_cutoff        p = !(p.map_cutoff)
let map_identity      p = !(p.map_identity)
let keep_at_most      p = !(p.keep_at_most)
let keep_factor       p = !(p.keep_factor)
let mrca_class        p = !(p.mrca_class)


(* arguments and preferences *)

let spec_with_default symbol setfun p help =
  (symbol, setfun p, Printf.sprintf help !p)

let specl prefs =
  align_with_space [
    (* short *)
"-c", Arg.Set_string prefs.refpkg_path,
"Specify the path to the reference package.";
"-t", Arg.Set_string prefs.tree_fname,
"Specify the reference tree filename.";
"-r", Arg.Set_string prefs.ref_align_fname,
"Specify the reference alignment filename.";
"-s", Arg.Set_string prefs.stats_fname,
"Supply a phyml stats.txt or a RAxML info file giving the model parameters.";
"-d", Arg.Set_string prefs.ref_dir,
"Specify the directory containing the reference information.";
"-p", Arg.Unit (fun () ->
  prefs.calc_pp := true;
  prefs.keep_at_most := 20;
  prefs.keep_factor := 0.001;
  prefs.max_strikes := 20),
"Calculate posterior probabilities.";
"-m", Arg.Set_string prefs.model_name,
"Substitution model. Protein: are LG, WAG, or JTT. Nucleotides: GTR.";
(* model *)
"--model-freqs", Arg.Clear prefs.emperical_freqs,
"Use model frequencies instead of reference alignment frequencies.";
"--gamma-cats", Arg.Set_int prefs.gamma_n_cat,
"Number of categories for discrete gamma model.";
"--gamma-alpha", Arg.Set_float prefs.gamma_alpha,
"Specify the shape parameter for a discrete gamma model.";
(* like calc parameters *)
spec_with_default "--ml-tolerance" (fun o -> Arg.Set_float o) prefs.initial_tolerance
"1st stage branch len optimization tolerance (2nd stage to 1e-5). Default: %g.";
spec_with_default "--pp-rel-err" (fun o -> Arg.Set_float o) prefs.pp_rel_err
"Relative error for the posterior probability calculation. Default is %g.";
"--unif-prior", Arg.Set prefs.uniform_prior,
"Use a uniform prior rather than exponential.";
"--inform-prior", Arg.Set prefs.informative_prior,
"Use an informative exponential prior based on rooted distance to leaves.";
spec_with_default "--prior-lower" (fun o -> Arg.Set_float o) prefs.prior_lower
"Lower bound for the informative prior mean. Default is %g.";
spec_with_default "--start-pend" (fun o -> Arg.Set_float o) prefs.start_pend
"Starting pendant branch length. Default is %g.";
spec_with_default "--max-pend" (fun o -> Arg.Set_float o) prefs.max_pend
"Set the maximum ML pendant branch length. Default is %g.";
(* baseball *)
spec_with_default "--fig-cutoff" (fun o -> Arg.Set_float o) prefs.fig_cutoff
"The cutoff for determining figs. Default is %g; specify 0 to disable.";
"--fig-eval-all", Arg.Set prefs.evaluate_all,
"Evaluate all likelihoods to ensure that the best location was selected.";
"--fig-eval-discrepancy-tree", Arg.Set_string prefs.evaluation_discrepancy,
"Write out a tree showing the discrepancies between the best complete and observed locations.";
"--fig-tree", Arg.Set_string prefs.fig_tree,
"Write out a tree showing the figs on the tree.";
spec_with_default "--max-strikes" (fun o -> Arg.Set_int o) prefs.max_strikes
"Maximum number of strikes for baseball. 0 -> no ball playing. Default is %d.";
spec_with_default "--strike-box" (fun o -> Arg.Set_float o) prefs.strike_box
"Set the size of the strike box in log likelihood units. Default is %g.";
spec_with_default "--max-pitches" (fun o -> Arg.Set_int o) prefs.max_pitches
"Set the maximum number of pitches for baseball. Default is %d.";
"--fantasy", Arg.Set_float prefs.fantasy,
"Desired likelihood cutoff for fantasy baseball mode. 0 -> no fantasy.";
spec_with_default "--fantasy-frac" (fun o -> Arg.Set_float o) prefs.fantasy_frac
"Fraction of fragments to use when running fantasy baseball. Default is %g.";
(* other *)
"--write-masked", Arg.Set prefs.write_masked,
"Write alignment masked to the region without gaps in the query.";
spec_with_default "--verbosity" (fun o -> Arg.Set_int o) Ppatteries.verbosity
"Set verbosity level. 0 is silent, and 2 is quite a lot. Default is %d.";
"--out-dir", Arg.Set_string prefs.out_dir,
"Specify the directory to write place files to.";
"--pretend", Arg.Set prefs.pretend,
"Only check out the files then report. Do not run the analysis.";
"--check-like", Arg.Set prefs.check_like,
"Write out the likelihood of the reference tree, calculated two ways.";
spec_with_default "-j" (fun o -> Arg.Set_int o) prefs.children
"The number of child processes to spawn when doing placements. Default is %d.";
"--timing", Arg.Set prefs.timing,
"Display timing information after the pplacer run finishes.";
"--no-pre-mask", Arg.Set prefs.no_pre_mask,
"Don't pre-mask sequences before placement.";
"--write-pre-masked", Arg.Set_string prefs.pre_masked_file,
"Write out the pre-masked sequences to the specified fasta file and exit.";
"--map-mrca", Arg.Set_string prefs.map_fasta,
"Specify a file to write out MAP sequences for MRCAs and corresponding placements.";
spec_with_default "--map-mrca-min" (fun o -> Arg.Set_float o) prefs.map_cutoff
"Specify cutoff for inclusion in MAP sequence file. Default is %g.";
"--map-identity", Arg.Set prefs.map_identity,
"Add the percent identity of the query sequence to the nearest MAP sequence to each placement.";
spec_with_default "--keep-at-most" (fun o -> Arg.Set_int o) prefs.keep_at_most
"The maximum number of placements we keep. Default is %d.";
spec_with_default "--keep-factor" (fun o -> Arg.Set_float o) prefs.keep_factor
"Throw away anything that has ml_ratio below keep_factor times (best ml_ratio). Default is %g.";
"--mrca-class", Arg.Set prefs.mrca_class,
"Classify with MRCAs instead of a painted tree.";
"--version", Arg.Set prefs.version,
"Write out the version number and exit.";
  ]

(* do a sanity check on the preferences *)
let check p =
  if start_pend p <= 0. then
    failwith "Starting pendant branch length must be strictly positive.";
  if start_pend p >= max_pend p then
    failwith "Starting pendant branch length must be strictly less than maximum pendant branch length.";
  ()

let usage =
    "pplacer [options] [alignment]"

  (* for the time being just to generate documentation *)
class pplacer_cmd () =
object
  inherit Subcommand.subcommand ()
  method desc = "performs phylogenetic placement"
  method usage = usage
  method specl = specl (defaults ())
  method action _ = ()
end



open Fam_batteries
open MapsSets

module Prefs = struct
  type mokaphy_prefs =
    {
      p_exp: float ref;
      use_pp: bool ref;
      transform : string ref;
      n_samples: int ref;
      out_fname: string ref;
      list_output: bool ref;
      density: bool ref;
      box_plot: bool ref;
      weighted: bool ref;
      seed: int ref;
      matrix: bool ref;
    }
 
  let use_pp            p = !(p.use_pp)
  let transform         p = !(p.transform)
  let n_samples         p = !(p.n_samples)
  let out_fname         p = !(p.out_fname)
  let list_output       p = !(p.list_output)
  let density           p = !(p.density)
  let box_plot          p = !(p.box_plot)
  let p_exp             p = !(p.p_exp)
  let weighted          p = !(p.weighted)
  let seed              p = !(p.seed)
  let matrix            p = !(p.matrix)
 
  let defaults () =
    {
      use_pp = ref false;
      verbose = ref false;
      normal = ref false;
      n_samples = ref 0;
      out_fname = ref "";
      list_output = ref false;
      density = ref false;
      p_plot = ref false;
      box_plot = ref false;
      p_exp = ref 1.;
      weighted = ref true;
      seed = ref 1;
      matrix = ref false;
      bary_density = ref false;
      ddensity = ref false;
      refpkg_path = ref "";
      transform = ref "";
    }

  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_fname,
    "Set the filename to write to. Otherwise write to stdout.";
    "-c", Arg.Set_string prefs.refpkg_path,
    (Mokaphy_common.refpkg_help "kr");
    "-p", Arg.Set prefs.use_pp,
    "Use posterior probability.";
    "--exp", Arg.Set_float prefs.p_exp,
    "The exponent for the integration, i.e. the value of p in Z_p.";
    "--unweighted", Arg.Clear prefs.weighted,
    Mokaphy_common.weighted_help;
    (*
    "--transform", Arg.Set_string prefs.transform,
    Mokaphy_common.transform_help;
    *)
    "--list-out", Arg.Set prefs.list_output,
    "Output the KR results as a list rather than a matrix.";
    "--density", Arg.Set prefs.density,
    "write out a shuffle density data file for each pair.";
    "--pplot", Arg.Set prefs.p_plot,
        "write out a plot of the distances when varying the p for the Z_p calculation";
    "--box", Arg.Set prefs.box_plot,
        "write out a box and point plot showing the original sample distances compared to the shuffled ones.";
    "-s", Arg.Set_int prefs.n_samples,
        ("Set how many samples to use for significance calculation (0 means \
        calculate distance only). Default is "^(string_of_int (n_samples prefs)));
    Mokaphy_common.spec_with_default "--seed" (fun o -> Arg.Set_int o) prefs.seed
    "Set the random seed, an integer > 0. Default is %d.";
    "--matrix", Arg.Set prefs.matrix,
    "Use the matrix formulation to calculate distance and p-value.";
    "--verbose", Arg.Set prefs.verbose,
    "Verbose running.";
]
end


type result =
  {
    distance : float;
    p_value : float option;
  }

let get_distance r = r.distance
let get_p_value r = match r.p_value with
  | Some p -> p
  | None -> failwith "no p-value!"

let pair_core transform p n_samples t pre1 pre2 =
  let resampled_dists =
    Normal_approx.normal_pair_approx rng weighting
      criterion (Mokaphy_prefs.n_samples prefs) p pr1 pr2
  in
  (* here we shadow original_dist with one we know is unweighted *)
  let original_dist =
    Kr_distance.pair_distance
      weighting
      criterion
      p
      pr1
      pr2
  in
  R_plots.write_density
    "normal"
    (Placerun.get_name pr1)
    (Placerun.get_name pr2)
    original_dist
    resampled_dists
    p;
  { distance = original_dist;
    p_value =
      Some
        (Mokaphy_base.list_onesided_pvalue
          resampled_dists
          original_dist)}

let normal_core ch prefs prl =
  if List.length prl = 0 then
    exit 0; (* zero mokaphy --help *)
  if List.length prl = 1 then
    invalid_arg "can't do KR with fewer than two place files";
  let n_samples = Prefs.n_samples prefs
  and is_weighted = Prefs.weighted prefs
  and use_pp = Prefs.use_pp prefs
  and pra = Array.of_list prl
  and p = Prefs.p_exp prefs
  and transform = Mass_map.transform_of_str (Prefs.transform prefs)
  in
  if n_samples <= 0 then
      failwith "Please specify some number of normal samples greater than 0.";
  (* below is for make_shuffled_pres *)
  Random.init (Prefs.seed prefs);
  (* the names of the placeruns *)
  and names = Array.map Placerun.get_name pra
  in
  Mokaphy_common.write_uptril
    (Prefs.list_output prefs)
    names
    [Uptri.map get_distance u; Uptri.map get_p_value u;]
    ch


let normal prefs prl =
  Mokaphy_common.wrap_output (Prefs.out_fname prefs)
    (fun ch -> normal_core ch prefs prl)

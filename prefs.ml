(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * preferences data type and function.
 *)

type prefs = 
  {
   (* basics *)
    tree_fname : string ref;
    ref_align_fname : string ref;
    stats_fname : string ref;
    ref_dir : string ref;
    out_dir : string ref;
    (* tree calc *)
    start_pend : float ref;
    max_pend : float ref;
    tolerance : float ref;
    calc_pp : bool ref;
    uniform_prior : bool ref;
    pp_rel_err : float ref;
    (* playing ball *)
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
    verb_level : int ref;
    write_masked : bool ref;
    only_write_best : bool ref;
    (* other *)
    friendly : bool ref;
    pretend : bool ref;
  }


(* default values *)
let defaults () = 
  { 
    (* basics *)
    tree_fname = ref "";
    ref_align_fname = ref "";
    stats_fname = ref "";
    ref_dir = ref ""; (* empty is the correct default; it gets some special handling *)
    out_dir = ref ".";
    (* tree calc *)
    start_pend = ref 0.1;
    max_pend = ref 2.;
    tolerance = ref 0.001;
    calc_pp = ref false;
    uniform_prior = ref false;
    pp_rel_err = ref 0.01;
    (* playing ball *)
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
    verb_level = ref 1;
    write_masked = ref false;
    only_write_best = ref false;
    (* other *)
    friendly = ref true;
    pretend = ref false;
  }


type mut_param = 
  | MutBool of bool ref
  | MutFloat of float ref
  | MutInt of int ref
  | MutString of string ref


(* yay visual block! *)
let tree_fname      p = !(p.tree_fname)
let ref_align_fname p = !(p.ref_align_fname)
let stats_fname     p = !(p.stats_fname)
let start_pend      p = !(p.start_pend)
let max_pend        p = !(p.max_pend)
let tolerance       p = !(p.tolerance)
let calc_pp         p = !(p.calc_pp)
let uniform_prior   p = !(p.uniform_prior)
let pp_rel_err      p = !(p.pp_rel_err)
let max_strikes     p = !(p.max_strikes)
let strike_box      p = !(p.strike_box)
let max_pitches     p = !(p.max_pitches)
let fantasy         p = !(p.fantasy)
let fantasy_frac    p = !(p.fantasy_frac)
let emperical_freqs p = !(p.emperical_freqs)
let model_name      p = !(p.model_name)
let gamma_n_cat     p = !(p.gamma_n_cat)
let gamma_alpha     p = !(p.gamma_alpha)
let verb_level      p = !(p.verb_level)
let write_masked    p = !(p.write_masked)
let only_write_best p = !(p.only_write_best)
let ref_dir         p = !(p.ref_dir)
let out_dir         p = !(p.out_dir)
let friendly        p = !(p.friendly)
let pretend         p = !(p.pretend)


(* arguments and preferences *)

let spec_with_default symbol setfun p help = 
  (symbol, setfun p, Printf.sprintf help !p)

let args prefs = 
  [
    (* short *)
    "-t", Arg.Set_string prefs.tree_fname,
    "Specify the reference tree filename.";
    "-r", Arg.Set_string prefs.ref_align_fname,
    "Specify the reference alignment filename.";
    "-s", Arg.Set_string prefs.stats_fname,
    "Supply a phyml stats.txt file or a RAxML info file which specifies the model parameters. \
    The information in this file can be overriden on the command line.";
    "-d", Arg.Set_string prefs.ref_dir,
    "Specify the directory containing the reference information.";
    "-p", Arg.Set prefs.calc_pp, 
    "Calculate posterior probabilities.";
    "-m", Arg.Set_string prefs.model_name,
    "Set the sequence substitution model. Protein options are LG (default) or WAG. \
    For nucleotides the GTR parameters must be specified via a stats file.";
    (* model *)
    "--modelFreqs", Arg.Clear prefs.emperical_freqs,
    "Use protein frequencies counted from the chosen model rather than counts \
    from the reference alignment.";
    "--gammaCats", Arg.Set_int prefs.gamma_n_cat,
    "Specify the number of categories for a discrete gamma model. (Default is \
    one, i.e. no gamma rate variation.)";
    "--gammaAlpha", Arg.Set_float prefs.gamma_alpha,
    "Specify the shape parameter for a discrete gamma model.";
    (* like calc parameters *)
    spec_with_default "--mlTolerance" (fun o -> Arg.Set_float o) prefs.tolerance
    "Specify the tolerance for the branch length optimization. Default is %g.";
    spec_with_default "--ppRelErr" (fun o -> Arg.Set_float o) prefs.pp_rel_err
    "Specify the relative error for the posterior probability calculation. Default is %g.";
    "--uniformPrior", Arg.Set prefs.uniform_prior,
    "Use a uniform prior rather than exponential in the posterior probability \
    calculation.";
    spec_with_default "--startPend" (fun o -> Arg.Set_float o) prefs.start_pend
    "Set the starting pendant branch length for the ML and Bayes calculations. Default is %g.";
    spec_with_default "--maxPend" (fun o -> Arg.Set_float o) prefs.max_pend
    "Set the maximum pendant branch length for the ML calculation. Default is %g.";
    (* baseball *)
    spec_with_default "--maxStrikes" (fun o -> Arg.Set_int o) prefs.max_strikes
    "Set the maximum number of strikes for baseball. Setting to zero disables ball playing. Default is %d.";
    spec_with_default "--strikeBox" (fun o -> Arg.Set_float o) prefs.strike_box
    "Set the size of the strike box in log likelihood units. Default is %g.";
    spec_with_default "--maxPitches" (fun o -> Arg.Set_int o) prefs.max_pitches
    "Set the maximum number of pitches for baseball. Default is %d.";
    spec_with_default "--fantasy" (fun o -> Arg.Set_float o) prefs.fantasy
    "Set to a nonzero value to run in fantasy baseball mode. The value given \
    will be the desired average difference between the likelihood of the best \
    placement with the given baseball parameters and that evaluating all \
    maxPitches pitches. Default is %g.";
    spec_with_default "--fantasyFrac" (fun o -> Arg.Set_float o) prefs.fantasy_frac
    "Set the fraction of fragments to use when running fantasy baseball. Default is %g.";
    (* other *)
    "--writeMasked", Arg.Set prefs.write_masked,
    "Write out the reference alignment with the query sequence, masked to the \
    region without gaps in the query.";
    spec_with_default "--verbosity" (fun o -> Arg.Set_int o) prefs.verb_level 
    "Set verbosity level. 0 is silent, and 2 is quite a lot. Default is %d.";
    "--unfriendly", Arg.Clear prefs.friendly,
    "Do not run friend finder pre-analysis.";
    "--outDir", Arg.Set_string prefs.out_dir,
    "Specify the directory to write place files to.";
    "--pretend", Arg.Set prefs.pretend,
    "Only check out the files then report. Do not run the analysis.";
  ]

(* include a pref here if it should go in the place file *)
let titled_typed_prefs p =
  [
    MutString p.tree_fname,      "reference tree file"         ; 
    MutString p.ref_align_fname, "reference alignment file"    ; 
    MutString p.stats_fname,     "statistics file"             ; 
    MutString p.ref_dir,         "reference data directory"    ; 
    MutString p.model_name,      "substitution model"          ; 
    MutBool p.emperical_freqs,   "use emperical frequencies"   ; 
    MutInt p.gamma_n_cat,        "number of gamma categories"  ; 
    MutFloat p.gamma_alpha,      "gamma alpha"                 ; 
    MutInt p.max_strikes,        "max number of strikes"       ; 
    MutFloat p.strike_box,       "strike box"                  ; 
    MutInt p.max_pitches,        "max number of pitches"       ; 
    MutBool p.calc_pp,           "calculate PP"                ; 
    MutBool p.uniform_prior,     "uniform prior"               ; 
    MutFloat p.start_pend,       "starting pendant length"     ; 
    MutFloat p.max_pend,         "maximal pendant length"      ; 
    MutFloat p.tolerance,        "ML tolerance"                ; 
    MutFloat p.pp_rel_err,       "relative error for PP"       ; 
  ]

(* do a sanity check on the preferences *)
let check p = 
  if start_pend p <= 0. then
    failwith "Starting pendant branch length must be strictly positive.";
  if start_pend p >= max_pend p then
    failwith "Starting pendant branch length must be strictly less than maximum pendant branch length.";
  ()

(* reading and writing *)

(* we can't further factor the match because it would have to return things of
 * different types *)
let write ch p = 
  List.iter 
    (fun (typed_pref, title) ->
      let write fmt pref = Printf.fprintf ch fmt title (!pref) in
      match typed_pref with
      | MutBool b -> write "# %s: %b\n" b
      | MutFloat x -> write "# %s: %g\n" x
      | MutInt i -> write "# %s: %d\n" i
      | MutString s -> write "# %s: %s\n" s)
    (titled_typed_prefs p)

(* let read_prefs line fmt title pref =  *)
let read pref_lines = 
  let p = defaults () in
  let rec aux = function
    | ((typed_pref, title)::more_prefs, line::more_lines) -> begin
      let scan fmt pref =
        Scanf.sscanf line fmt
          (fun found_title x ->
            if title = found_title then
              pref := x
            else
              failwith 
                ("Prefs.read: expected '"^title^"', found '"^found_title^"'"));
        (* recur here *)
        aux (more_prefs, more_lines)
      in
      match typed_pref with
      (* @: means stop reading at the colon. otherwise will slurp whole str *)
      | MutBool b -> scan "# %s@: %b" b
      | MutFloat x -> scan "# %s@: %g" x
      | MutInt i -> scan "# %s@: %d" i
      | MutString s -> scan "# %s@: %s" s
    end
    | (_::_, []) -> failwith "Prefs.read: not enough pref lines!"
    | _ -> ()
  in
  aux (titled_typed_prefs p, pref_lines);
  p


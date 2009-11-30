(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

type mokaphy_prefs = 
  {
    verbose: bool ref;
    shuffle: bool ref;
    n_samples: int ref;
    out_fname: string ref;
    density: bool ref;
    p_plot: bool ref;
    box_plot: bool ref;
    p_exp: float ref;
    weighted: bool ref;
    matrix_check: bool ref;
    ddensity: bool ref;
    heat_tree: bool ref;
    white_bg: bool ref;
    bary_prefix: string ref;
  }

let verbose      p = !(p.verbose)
let shuffle      p = !(p.shuffle)
let out_fname    p = !(p.out_fname)
let n_samples    p = !(p.n_samples)
let density      p = !(p.density)
let p_plot       p = !(p.p_plot)
let box_plot     p = !(p.box_plot)
let p_exp        p = !(p.p_exp)
let weighted     p = !(p.weighted)
let matrix_check p = !(p.matrix_check)
let ddensity     p = !(p.ddensity)
let heat_tree    p = !(p.heat_tree)
let white_bg    p = !(p.white_bg)
let bary_prefix  p = !(p.bary_prefix)


(* defaults *)
let defaults () = 
  { 
    verbose = ref false;
    shuffle = ref true;
    out_fname = ref "";
    n_samples = ref 0;
    density = ref false;
    p_plot = ref false;
    box_plot = ref false;
    p_exp = ref 1.;
    weighted = ref true;
    matrix_check = ref false;
    ddensity = ref false;
    heat_tree = ref false;
    white_bg = ref false;
    bary_prefix = ref "";
  }


(* arguments *)
let args prefs = [
  "-v", Arg.Set prefs.verbose,
  "verbose running.";
  "--normal", Arg.Clear prefs.shuffle,
  "Use the normal approximation rather than shuffling. This disables the --pplot and --box options if set.";
  "-p", Arg.Set_float prefs.p_exp,
  "The value of p in Z_p.";
  "--unweighted", Arg.Clear prefs.weighted,
      "The unweighted version simply uses the best placement. Default is weighted.";
  "--density", Arg.Set prefs.density,
  "write out a shuffle density data file for each pair.";
  "--pplot", Arg.Set prefs.p_plot,
      "write out a plot of the distances when varying the p for the Z_p calculation";
  "--box", Arg.Set prefs.box_plot,
      "write out a box and point plot showing the original sample distances compared to the shuffled ones.";
  "-o", Arg.Set_string prefs.out_fname,
  "Set the filename to write to. Otherwise write to stdout.";
  "-s", Arg.Set_int prefs.n_samples,
      ("Set how many samples to use for significance calculation (0 means \
      calculate distance only). Default is "^(string_of_int (n_samples prefs)));
  "--matrix", Arg.Set prefs.matrix_check,
      "Run a check using the distance matrix formulation of the KR p=2 distance.";
  "--ddensity", Arg.Set prefs.ddensity,
    "Make distance-by-distance densities.";
  "--heat", Arg.Set prefs.heat_tree,
  "Make a heat tree for each pair.";
  "--whitebg", Arg.Set prefs.white_bg,
  "Make colors for the heat tree which are compatible with a white background.";
  "--bary", Arg.Set_string prefs.bary_prefix,
  "Calculate the barycenter of each of the .place files. Specify the prefix
  string"
  ]


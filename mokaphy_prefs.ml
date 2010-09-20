(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * This should have no nontrivial dependencies, so that it can be used anywhere.
 *)

let spec_with_default symbol setfun p help = 
  (symbol, setfun p, Printf.sprintf help !p)

let weighted_help = 
  "The point version simply uses the best placement, rather than spreading out the probability mass. Default is spread.";

(* Bary Bary Bary Bary Bary Bary Bary Bary *)
module Bary = struct
  type prefs = 
    {
      out_fname: string ref;
      use_pp: bool ref;
      weighted: bool ref;
    }

  let out_fname         p = !(p.out_fname)
  let use_pp            p = !(p.use_pp)
  let weighted          p = !(p.weighted)

  let defaults () =
    {
      out_fname = ref "";
      use_pp = ref false;
      weighted = ref true;
    }

  let specl_of_prefs prefs = 
[
  "-p", Arg.Set prefs.use_pp,
  "Use posterior probability.";
  "--point", Arg.Clear prefs.weighted,
  weighted_help;
  "-o", Arg.Set_string prefs.out_fname,
  "Set the filename to write to. Otherwise write to stdout.";
]
end

(* Heat Heat Heat Heat Heat Heat Heat Heat *)
module Heat = struct
  type mokaphy_prefs = 
    {
      out_fname: string ref;
      use_pp: bool ref;
      p_exp: float ref;
      weighted: bool ref;
      simple_colors: bool ref;
      gray_black_colors: bool ref;
      white_bg: bool ref;
      gray_level : int ref;
      min_width : float ref;
      max_width : float ref;
    }
  
  let out_fname         p = !(p.out_fname)
  let use_pp            p = !(p.use_pp)
  let p_exp             p = !(p.p_exp)
  let weighted          p = !(p.weighted)
  let simple_colors     p = !(p.simple_colors)
  let gray_black_colors p = !(p.gray_black_colors)
  let white_bg          p = !(p.white_bg)
  let gray_level        p = !(p.gray_level)
  let min_width         p = !(p.min_width)
  let max_width         p = !(p.max_width)
  
  let defaults () = 
    { 
      out_fname = ref "";
      use_pp = ref false;
      p_exp = ref 1.;
      weighted = ref true;
      simple_colors = ref false;
      gray_black_colors = ref false;
      white_bg = ref false;
      gray_level = ref 5;
      min_width = ref 0.5;
      max_width = ref 13.;
    }
  
  let specl_of_prefs prefs = 
[
"-o", Arg.Set_string prefs.out_fname,
"Set the filename to write to. Otherwise write to stdout.";
"-p", Arg.Set prefs.use_pp,
"Use posterior probability.";
"--exp", Arg.Set_float prefs.p_exp,
"The exponent for the integration, i.e. the value of p in Z_p.";
"--point", Arg.Clear prefs.weighted,
weighted_help;
"--simple-colors", Arg.Set prefs.simple_colors,
"Use only 100% red and blue to signify the sign of the KR along that edge.";
"--gray-black", Arg.Set prefs.gray_black_colors,
"Use gray and black in place of red and blue to signify the sign of the KR along that edge.";
"--white-bg", Arg.Set prefs.white_bg,
"Make colors for the heat tree which are compatible with a white background.";
spec_with_default "--gray-level" (fun o -> Arg.Set_int o) prefs.gray_level
"Specify the amount of gray to mix into the color scheme. Default is %d.";
spec_with_default "--min-width" (fun o -> Arg.Set_float o) prefs.min_width
"Specify the minimum width of the branches in a heat tree. Default is %g.";
spec_with_default "--max-width" (fun o -> Arg.Set_float o) prefs.max_width
"Specify the maximum width of the branches in a heat tree. Default is %g.";
]
end



(* KR KR KR KR KR KR KR KR *)
module KR = struct
  type mokaphy_prefs = 
    {
      use_pp: bool ref;
      verbose: bool ref;
      normal: bool ref;
      n_samples: int ref;
      out_fname: string ref;
      list_output: bool ref;
      density: bool ref;
      p_plot: bool ref;
      box_plot: bool ref;
      p_exp: float ref;
      weighted: bool ref;
      seed: int ref;
      matrix: bool ref;
      bary_density: bool ref;
      ddensity: bool ref;
    }
  
  let use_pp            p = !(p.use_pp)
  let verbose           p = !(p.verbose)
  let normal            p = !(p.normal)
  let n_samples         p = !(p.n_samples)
  let out_fname         p = !(p.out_fname)
  let list_output       p = !(p.list_output)
  let density           p = !(p.density)
  let p_plot            p = !(p.p_plot)
  let box_plot          p = !(p.box_plot)
  let p_exp             p = !(p.p_exp)
  let weighted          p = !(p.weighted)
  let seed              p = !(p.seed)
  let matrix            p = !(p.matrix)
  let bary_density      p = !(p.bary_density)
  let ddensity          p = !(p.ddensity)
  
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
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_fname,
    "Set the filename to write to. Otherwise write to stdout.";
    "--verbose", Arg.Set prefs.verbose,
    "Verbose running.";
    "-p", Arg.Set prefs.use_pp,
    "Use posterior probability.";
    "--exp", Arg.Set_float prefs.p_exp,
    "The exponent for the integration, i.e. the value of p in Z_p.";
    "--unweighted", Arg.Clear prefs.weighted,
    weighted_help;
    "--list-output", Arg.Set prefs.list_output,
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
    "--seed", Arg.Set_int prefs.seed,
    "Set the random seed, an integer > 0.";
    (*
    "--normal", Arg.Set prefs.normal,
    "Use the normal approximation rather than shuffling. This disables the --pplot and --box options if set.";
    *)
    "--bary-density", Arg.Set prefs.bary_density,
    "Write out a density plot of barycenter distance versus shuffled version for each pair.";
    (*
    "--matrix", Arg.Set prefs.matrix,
    "Use the matrix formulation to calculate distance and p-value.";
    *)
    "--ddensity", Arg.Set prefs.ddensity,
      "Make distance-by-distance densities.";
    ]
end 


(* PD PD PD PD PD PD PD PD *)
module PD = struct
  type mokaphy_prefs = 
    {
      use_pp: bool ref;
      out_fname: string ref;
    }
  
  let use_pp            p = !(p.use_pp)
  let out_fname         p = !(p.out_fname)
  
  let defaults () = 
    { 
      use_pp = ref false;
      out_fname = ref "";
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_fname,
    "Set the filename to write to. Otherwise write to stdout.";
    "-p", Arg.Set prefs.use_pp,
    "Use posterior probability.";
    ]
end 

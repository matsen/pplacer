(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

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
  "--unweighted", Arg.Clear prefs.weighted,
  "The unweighted version simply uses the best placement. Default is weighted.";
  "-o", Arg.Set_string prefs.out_fname,
  "Set the filename to write to. Otherwise write to stdout.";
]
end


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
    }
  
  let out_fname         p = !(p.out_fname)
  let use_pp            p = !(p.use_pp)
  let p_exp             p = !(p.p_exp)
  let weighted          p = !(p.weighted)
  let simple_colors     p = !(p.simple_colors)
  let gray_black_colors p = !(p.gray_black_colors)
  let white_bg          p = !(p.white_bg)
  
  let defaults () = 
    { 
      out_fname = ref "";
      use_pp = ref false;
      p_exp = ref 1.;
      weighted = ref true;
      simple_colors = ref false;
      gray_black_colors = ref false;
      white_bg = ref false;
    }
  
  let specl_of_prefs prefs = 
[
"-o", Arg.Set_string prefs.out_fname,
"Set the filename to write to. Otherwise write to stdout.";
"-p", Arg.Set prefs.use_pp,
"Use posterior probability.";
"--exp", Arg.Set_float prefs.p_exp,
"The exponent for the integration, i.e. the value of p in Z_p.";
"--unweighted", Arg.Clear prefs.weighted,
    "The unweighted version simply uses the best placement. Default is weighted.";
"--simpleColors", Arg.Set prefs.simple_colors,
"Use only 100% red and blue to signify the sign of the KR along that edge.";
"--grayBlackColors", Arg.Set prefs.gray_black_colors,
"Use gray and black in place of red and blue to signify the sign of the KR along that edge.";
"--whitebg", Arg.Set prefs.white_bg,
"Make colors for the heat tree which are compatible with a white background.";
]
end


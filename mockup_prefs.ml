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







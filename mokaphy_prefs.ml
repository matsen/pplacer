(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

type mokaphy_prefs = 
  {
    verbose: bool ref;
    n_shuffles: int ref;
    out_fname: string ref;
    histo: bool ref;
    p_plot: bool ref;
    p_exp: float ref;
    dropdown: float ref;
  }

let verbose    p = !(p.verbose)
let out_fname  p = !(p.out_fname)
let n_shuffles p = !(p.n_shuffles)
let histo      p = !(p.histo)
let p_plot     p = !(p.p_plot)
let p_exp      p = !(p.p_exp)
let dropdown   p = !(p.dropdown)

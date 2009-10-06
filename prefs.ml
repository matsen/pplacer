(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

type prefs = 
  {
   (* basics *)
    tree_fname : string ref;
    ref_align_fname : string ref;
    stats_fname : string ref;
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
    (* model *)
    emperical_freqs : bool ref;
    model_name : string ref;
    gamma_n_cat : int ref;
    gamma_alpha : float ref;
    (* reading and writing *)
    verb_level : int ref;
    write_masked : bool ref;
    ratio_cutoff : float ref;
    only_write_best : bool ref;
  }

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
  let emperical_freqs p = !(p.emperical_freqs)
  let model_name      p = !(p.model_name)
  let gamma_n_cat     p = !(p.gamma_n_cat)
  let gamma_alpha     p = !(p.gamma_alpha)
  let verb_level      p = !(p.verb_level)
  let write_masked    p = !(p.write_masked)
  let ratio_cutoff    p = !(p.ratio_cutoff)
  let only_write_best p = !(p.only_write_best)

let titled_flt ch title f = 
  Printf.fprintf ch "# %s: %g\n" title f

let titled_int ch title i =
  Printf.fprintf ch "# %s: %d\n" title i

let titled_str ch title s =
  Printf.fprintf ch "# %s: %s\n" title s

let titled_boo ch title b =
  Printf.fprintf ch "# %s: %b\n" title b

let write_prefs ch p = 
  titled_str ch "reference tree file"         (tree_fname p);
  titled_str ch "reference alignment file"    (ref_align_fname p);
  titled_str ch "statistics file"             (stats_fname p);
  titled_str ch "substitution model"          (model_name p);
  titled_boo ch "use emperical frequencies"   (emperical_freqs p);
  titled_int ch "number of gamma categories"  (gamma_n_cat p);
  titled_flt ch "gamma alpha"                 (gamma_alpha p);
  titled_int ch "max number of strikes"       (max_strikes p);
  titled_flt ch "strike box"                  (strike_box p);
  titled_int ch "max number of pitches"       (max_pitches p);
  titled_boo ch "calculate PP"                (calc_pp p);
  titled_boo ch "uniform prior"               (uniform_prior p);
  titled_flt ch "starting pendant length"     (start_pend p);
  titled_flt ch "maximal pendant length"      (max_pend p);
  titled_flt ch "ML tolerance"                (tolerance p);
  titled_flt ch "relative error for PP"       (pp_rel_err p);
  titled_flt ch "ML ratio cutoff for writing" (ratio_cutoff p);
  ()

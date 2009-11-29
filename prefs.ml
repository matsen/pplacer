(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
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
    fantasy : bool ref;
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


(* default values *)
let defaults () = 
  { 
    (* basics *)
    tree_fname = ref "";
    ref_align_fname = ref "";
    stats_fname = ref "";
    ref_dir = ref ".";
    (* tree calc *)
    start_pend = ref 0.5;
    max_pend = ref 2.;
    tolerance = ref 0.01;
    calc_pp = ref false;
    uniform_prior = ref false;
    pp_rel_err = ref 0.01;
    (* playing ball *)
    max_strikes = ref 6;
    strike_box = ref 3.;
    max_pitches = ref 40;
    fantasy = ref false;
    (* model *)
    emperical_freqs = ref true;
    model_name = ref "LG";
    gamma_n_cat = ref 1;
    gamma_alpha = ref 1.;
    (* reading and writing *)
    verb_level = ref 1;
    write_masked = ref false;
    ratio_cutoff = ref 0.05;
    only_write_best = ref false;
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
let emperical_freqs p = !(p.emperical_freqs)
let model_name      p = !(p.model_name)
let gamma_n_cat     p = !(p.gamma_n_cat)
let gamma_alpha     p = !(p.gamma_alpha)
let verb_level      p = !(p.verb_level)
let write_masked    p = !(p.write_masked)
let ratio_cutoff    p = !(p.ratio_cutoff)
let only_write_best p = !(p.only_write_best)
let ref_dir         p = !(p.ref_dir)

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
    MutFloat p.ratio_cutoff,     "ML ratio cutoff for writing" ; 
  ]

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


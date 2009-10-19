(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * to do:
 * fix seeds
 *)

open Fam_batteries
open MapsSets
open Placement
open Mokaphy_prefs

let version_str = "v0.3"

let prefs = 
  { 
    verbose = ref false;
    shuffle = ref true;
    out_fname = ref "";
    n_samples = ref 100;
    histo = ref true;
    p_plot = ref true;
    box_plot = ref false;
    p_exp = ref 1.;
    weighted = ref true;
  }

let parse_args () =
  let files  = ref [] in
  let verbose_opt = "-v", Arg.Set prefs.verbose,
    "verbose running."
  and normal_approx_opt = "--normalApprox", Arg.Clear prefs.shuffle,
    "Use the normal approximation rather than shuffling. This disables the --pPlot and --boxPlot options."
  and p_opt = "-p", Arg.Set_float prefs.p_exp,
    "The value of p in Z_p."
  and unweighted_opt = "--unweighted", Arg.Clear prefs.weighted,
    "The unweighted version simply uses the best placement. Default is weighted."
  and histo_opt = "--histo", Arg.Set prefs.histo,
    "write out a shuffle histogram data file for each pair."
  and p_plot_opt = "--pPlot", Arg.Set prefs.p_plot,
    "write out a plot of the distances when varying the p for the Z_p calculation"
  and box_plot_opt = "--boxPlot", Arg.Set prefs.box_plot,
    "write out a box and point plot showing the original sample distances compared to the shuffled ones."
  and out_fname_opt = "-o", Arg.Set_string prefs.out_fname,
    "Set the filename to write to. Otherwise write to stdout."
  and n_samples_opt = "-s", Arg.Set_int prefs.n_samples,
    ("Set how many samples to use for significance calculation (0 means \
    calculate distance only). Default is "^(string_of_int (n_samples prefs)))
  in
  let usage =
    "mokaphy "^version_str^"\nmokaphy ex1.place ex2.place...\n"
  and anon_arg arg =
    files := arg :: !files in
  let args = [verbose_opt; out_fname_opt; normal_approx_opt; n_samples_opt; histo_opt; p_plot_opt; p_opt; box_plot_opt; unweighted_opt ] in
  Arg.parse args anon_arg usage;
  List.rev !files

     
let () =
  if not !Sys.interactive then begin
    let fnames = parse_args () in
    let parsed = 
      List.map 
        (fun fname -> Placerun_io.parse_place_file fname)
        fnames
    in
    if parsed = [] then exit 0;
    List.iter
      (fun placerun -> 
        if Placerun.contains_unplaced_queries placerun then
          failwith 
            ((Placerun.get_name placerun)^
              " contains unplaced query sequences!"))
      parsed;
    let out_ch = 
      if out_fname prefs = "" then stdout
      else open_out (out_fname prefs)
    in
    Mokaphy_core.core
      prefs
      Placement.ml_ratio (* sorting criterion *)
      out_ch
      (Array.of_list parsed);
    if out_fname prefs <> "" then close_out out_ch
  end

(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

(*
for each edge, collect placements as a list of 
(location along edge, vector showing which one it is)
then take sums of these, and multiply them by length of segment

keep in mind:
at some point we may want to customize weight of the placements
we may or may not want to have the pendant branch lengths
we want to make a randomization procedure

*)
open Fam_batteries
open MapsSets
open Mokaphy_prefs


(*
let pair_core prefs criterion ref_tree placerun1 placerun2 = 
  let context = 
    Printf.sprintf 
      "comparing %s with %s" 
      (Placerun.get_name placerun1)
      (Placerun.get_name placerun2)
  and combined = Placerun.combine "combined" placerun1 placerun2
  in
  let ref_tree = Placerun.get_ref_tree combined in
  let ref_tree_len = Itree.tree_length ref_tree in
  try
    (* Printf.fprintf ch "%s\t%s\t%g\n" npcl_name1 npcl_name2 grand_total; *)
    let placement_arr = Array.of_list (npcl1 @ npcl2)
    and list_of_sub_array a start len = 
      Array.to_list (Array.sub a start len)
    in
    let to_shuffle = Array.copy placement_arr in
    let calc_dist places p = 
        pair_distance criterion ref_tree p
            (list_of_sub_array places 0 (List.length npcl1))
            (list_of_sub_array places 
              (List.length npcl1) (List.length npcl2)) 
    in
    (* calculate unshuffled distance *)
    let chosen_p = p_exp prefs in
    let sample_dist = calc_dist placement_arr chosen_p in
    let shuff_dists =
      Array.init 
        (n_shuffles prefs)
        (fun _ ->
          Base.shuffle to_shuffle;
          calc_dist to_shuffle chosen_p)
    in
    Array.sort compare shuff_dists;
    (* maybe write out histogram files *)
    if histo prefs then begin
      let histo_prefix = "histo."^npcl_name1^".VS."^npcl_name2 in
      (* the data *)
      let histo_dat_name = histo_prefix^".dat" in
      let histo_dat_ch = open_out histo_dat_name in
      Array.iter 
        (fun x -> Printf.fprintf histo_dat_ch "%g\n" x) 
        shuff_dists;
      close_out histo_dat_ch;
      (* the r file *)
      let histo_r_ch = open_out (histo_prefix^".r") in
      Printf.fprintf histo_r_ch "pdf(\"%s\")\n" (histo_prefix^".pdf");
      Printf.fprintf histo_r_ch "data <- read.table(\"%s\")\n" histo_dat_name;
      Printf.fprintf histo_r_ch 
                     "hist(data[,1], main=\"d(%s,%s) = %f\", xlab=\"KR Z_%g distance\")\n" 
                     npcl_name1 npcl_name2 sample_dist chosen_p;
      Printf.fprintf histo_r_ch "abline(v=%g, col=\"red\")\n" sample_dist;
      Printf.fprintf histo_r_ch "dev.off()\n";
      close_out histo_r_ch;
    end;
    if p_plot prefs then begin
      let p_plot_prefix = "p_plot."^npcl_name1^".VS."^npcl_name2 in
      (* the data *)
      let p_plot_dat_name = p_plot_prefix^".dat" in
      let p_plot_dat_ch = open_out p_plot_dat_name in
      let n_samples = 101
      and min_sample = 1e-3
      and max_sample = 1e+3 in
      let p_arr = 
        Base.logarithmically_evenly_spaced 
          n_samples min_sample max_sample in
      Array.iter 
        (fun p -> 
          Printf.fprintf 
            p_plot_dat_ch 
            "%g\t%g\n" 
            p 
            ((calc_dist placement_arr p) /. 
             (ref_tree_len ** (outer_exponent p))))
        p_arr;
      close_out p_plot_dat_ch;
      (* the r file *)
      let p_plot_r_ch = open_out (p_plot_prefix^".r") in
      Printf.fprintf p_plot_r_ch "pdf(\"%s\")\n" (p_plot_prefix^".pdf");
      Printf.fprintf p_plot_r_ch "data <- read.table(\"%s\")\n" p_plot_dat_name;
      Printf.fprintf p_plot_r_ch 
                     "plot(data, main=\"%s versus %s\", xlab=\"p\", ylab=\"normalized KR p-distance\", log=\"x\")\n" 
                     npcl_name1 npcl_name2;
      (* Printf.fprintf p_plot_r_ch "abline(v=1, col=\"red\")\n"; *)
      Printf.fprintf p_plot_r_ch "dev.off()\n";
      close_out p_plot_r_ch;
    end;

    let p_value = Base.arr_onesided_pvalue shuff_dists sample_dist in
    (* Printf.fprintf ch "%s\t%s\t%g\t%g\n" npcl_name1 npcl_name2 sample_dist
     * p_value; *)
    (sample_dist, p_value)
  with
  | Invalid_place_loc a -> 
      invalid_arg
        (Printf.sprintf 
          "%g is not a valid placement location when %s" a context)
  | Unplaced_sequences ->
      invalid_arg ("Unplaced sequences when "^context)
  | Total_kr_not_zero tkr ->
      failwith ("total kr_vect not zero for "^context^": "^(string_of_float tkr))
*)


let pair_core criterion weighting placerun1 placerun2 =
  Placerun_distance.pair_dist criterion weighting placerun1 placerun2

(* core
 * run pair_core for each unique pair 
 *)
let core prefs criterion ch placerun_arr = 
  Printf.printf "calculating Z_%g distance...\n" (p_exp prefs);
  let weighting = Placerun_distance.Unweighted in
  let u = 
    Uptri.init
      (Array.length placerun_arr)
      (fun i j ->
        pair_core 
          criterion
          weighting
          (p_exp prefs)
          placerun_arr.(i) 
          placerun_arr.(j))
  in
  let names = Array.map Placerun.get_name placerun_arr in
  Printf.fprintf ch "distances\n"; 
  Mokaphy_base.write_named_float_uptri ch names u
  (*
  write_named_float_uptri ch names (Uptri.map fst u);
  Printf.fprintf ch "\np-values\n"; 
  write_named_float_uptri ch names (Uptri.map snd u);
  *)

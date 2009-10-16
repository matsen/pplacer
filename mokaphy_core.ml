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


(* histogram shows where the sample sits in the shuffled distances *)
let write_histogram name1 name2 sample_dist shuffled_dists p =
  let histo_prefix = "histo."^name1^".VS."^name2 in
  (* the data *)
  let histo_dat_name = histo_prefix^".dat" in
  let histo_dat_ch = open_out histo_dat_name in
  List.iter 
    (fun x -> Printf.fprintf histo_dat_ch "%g\n" x) 
    shuffled_dists;
  close_out histo_dat_ch;
  (* the r file *)
  let histo_r_ch = open_out (histo_prefix^".r") in
  Printf.fprintf histo_r_ch "pdf(\"%s\")\n" (histo_prefix^".pdf");
  Printf.fprintf histo_r_ch "data <- read.table(\"%s\")\n" histo_dat_name;
  Printf.fprintf histo_r_ch 
    "hist(data[,1], main=\"d(%s,%s) = %f\", xlab=\"KR Z_%g distance\")\n" 
    name1 name2 sample_dist p;
  Printf.fprintf histo_r_ch "abline(v=%g, col=\"red\")\n" sample_dist;
  Printf.fprintf histo_r_ch "dev.off()\n";
  close_out histo_r_ch;
  ()

(* shows the distances for various p *)
let write_p_plot criterion weighting pr1 pr2 = 
  let p_plot_prefix = 
    "p_plot."^(Placerun.get_name pr1)^".VS."^(Placerun.get_name pr2) in
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
        (Placerun_distance.pair_dist criterion weighting p pr1 pr2))
    p_arr;
  close_out p_plot_dat_ch;
  (* the r file *)
  let p_plot_r_ch = open_out (p_plot_prefix^".r") in
  Printf.fprintf p_plot_r_ch "pdf(\"%s\")\n" (p_plot_prefix^".pdf");
  Printf.fprintf p_plot_r_ch "data <- read.table(\"%s\")\n" p_plot_dat_name;
  Printf.fprintf p_plot_r_ch 
    "plot(data, main=\"%s versus %s\", xlab=\"p\", ylab=\"normalized KR p-distance\", log=\"x\")\n" 
    (Placerun.get_name pr1) (Placerun.get_name pr2);
  Printf.fprintf p_plot_r_ch "dev.off()\n";
  close_out p_plot_r_ch;
  ()


(* makes an array of shuffled placeruns (identity of being in first or second
 * one shuffled randomly, but number in each the same) *)
let make_shuffled_prs n_shuffles pr1 pr2 = 
  let pq1 = Placerun.get_pqueries pr1
  and pq2 = Placerun.get_pqueries pr2
  in
  let pquery_arr = Array.of_list (pq1 @ pq2)
  and n1 = List.length pq1
  and n2 = List.length pq2
  in
  let pquery_sub start len = 
    Array.to_list (Array.sub pquery_arr start len)
  in
  let make_pr pr num pqueries = 
    Placerun.make
      (Placerun.get_ref_tree pr)
      ((Placerun.get_name pr)^"_shuffle_"^(string_of_int num))
      pqueries
  in
  ListFuns.init 
    n_shuffles
    (fun num ->
      Base.shuffle pquery_arr;
      (make_pr pr1 num (pquery_sub 0 n1),
      make_pr pr2 num (pquery_sub n1 n2)))

let pair_core prefs criterion pr1 pr2 =
  let weighting = 
    if Mokaphy_prefs.weighted prefs then Placerun_distance.Weighted 
    else Placerun_distance.Unweighted 
  in
  let shuffled_list = 
    make_shuffled_prs (Mokaphy_prefs.n_shuffles prefs) pr1 pr2 in
  let calc_dist = 
    Placerun_distance.pair_dist 
      criterion 
      weighting 
      (Mokaphy_prefs.p_exp prefs) in
  let sample_dist = calc_dist pr1 pr2 in
  let shuffled_dists = 
    List.map 
      (fun (spr1,spr2) -> calc_dist spr1 spr2)
      shuffled_list
  in
  if Mokaphy_prefs.histo prefs then
    write_histogram 
      (Placerun.get_name pr1)
      (Placerun.get_name pr2)
      sample_dist 
      shuffled_dists 
      (Mokaphy_prefs.p_exp prefs);
  if Mokaphy_prefs.p_plot prefs then
    write_p_plot criterion weighting pr1 pr2;
  let p_value = 
    Base.list_onesided_pvalue shuffled_dists sample_dist in
  (sample_dist, p_value)

(* core
 * run pair_core for each unique pair 
 *)
let core prefs criterion ch pr_arr = 
  Printf.printf "calculating Z_%g distance...\n" 
                (Mokaphy_prefs.p_exp prefs);
  let u = 
    Uptri.init
      (Array.length pr_arr)
      (fun i j ->
        pair_core 
          prefs
          criterion
          pr_arr.(i) 
          pr_arr.(j))
  in
  let names = Array.map Placerun.get_name pr_arr in
  Printf.fprintf ch "distances\n"; 
  Mokaphy_base.write_named_float_uptri ch names (Uptri.map fst u);
  Printf.fprintf ch "\np-values\n"; 
  Mokaphy_base.write_named_float_uptri ch names (Uptri.map snd u);

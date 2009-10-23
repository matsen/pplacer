(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Fam_batteries

let int_div x y = (float_of_int x) /. (float_of_int y)

(* histogram shows where the sample sits in the shuffled distances *)
let write_histogram 
      plot_name name1 name2 sample_dist shuffled_dists p =
  let prefix = plot_name^"."^name1^".VS."^name2 in
  (* the data *)
  let dat_name = prefix^".dat" in
  let dat_ch = open_out dat_name in
  List.iter 
    (fun x -> Printf.fprintf dat_ch "%g\n" x) 
    shuffled_dists;
  close_out dat_ch;
  (* the r file *)
  let r_ch = open_out (prefix^".r") in
  Printf.fprintf r_ch "pdf(\"%s\")\n" (prefix^".pdf");
  Printf.fprintf r_ch "data <- read.table(\"%s\")\n" dat_name;
  Printf.fprintf r_ch 
    "hist(data[,1], main=\"d(%s,%s) = %f\", xlab=\"KR Z_%g distance\")\n" 
    name1 name2 sample_dist p;
  Printf.fprintf r_ch "abline(v=%g, col=\"red\")\n" sample_dist;
  Printf.fprintf r_ch "dev.off()\n";
  close_out r_ch;
  ()

(* shows the distances for various p *)
let write_p_plot criterion weighting pr1 pr2 = 
  let prefix = 
    "p_plot."^(Placerun.get_name pr1)^".VS."^(Placerun.get_name pr2) in
  (* the data *)
  let dat_name = prefix^".dat" in
  let dat_ch = open_out dat_name in
  let n_samples = 101
  and min_p = 1e-3
  and max_p = 1e+3 in
  let p_arr = 
    Mokaphy_base.logarithmically_evenly_spaced 
      n_samples min_p max_p in
  Array.iter 
    (fun p -> 
      Printf.fprintf 
        dat_ch 
        "%g\t%g\n" 
        p 
        (Placerun_distance.pair_dist criterion weighting p pr1 pr2))
    p_arr;
  close_out dat_ch;
  (* the r file *)
  let r_ch = open_out (prefix^".r") in
  Printf.fprintf r_ch "pdf(\"%s\")\n" (prefix^".pdf");
  Printf.fprintf r_ch "data <- read.table(\"%s\")\n" dat_name;
  Printf.fprintf r_ch 
    "plot(data, main=\"%s versus %s\", xlab=\"p\", ylab=\"normalized KR p-distance\", log=\"x\")\n" 
    (Placerun.get_name pr1) (Placerun.get_name pr2);
  Printf.fprintf r_ch "dev.off()\n";
  close_out r_ch;
  ()


(* boxplot showing the shuffled p-distances and a point showing the orig data *)
let write_boxplot criterion weighting pr1 pr2 shuffled_prs =
  let prefix = "box."^(Placerun.get_name pr1)^".VS."^(Placerun.get_name pr2) in
  let n_samples = 11 in
  let min_p = 2. ** (int_div (1-n_samples) 2)
  and max_p = 2. ** (int_div (n_samples-1) 2) in
  let p_arr = 
    Mokaphy_base.logarithmically_evenly_spaced 
      n_samples min_p max_p in
  let calc_dist p x_pr1 x_pr2 = 
    Placerun_distance.pair_dist criterion weighting p x_pr1 x_pr2 in
  let write_dist ch p d = Printf.fprintf ch "%g\t%g\n" p d in
  let shuff_results =
    Array.map 
      (fun p -> 
        (p,
          List.map 
            (fun (spr1, spr2) -> calc_dist p spr1 spr2)
            shuffled_prs))
      p_arr
  in
  let stats = 
    Array.map 
      (fun (_,dists) -> Mokaphy_base.mean_std_dev dists)
      shuff_results;
  in
  let scale_dist (mu, sigma) d = (d -. mu) /. sigma in
  let scaled_orig = 
    ArrayFuns.map2 
      (fun stat (p,d) -> (p, scale_dist stat d))
      stats 
      (Array.map (fun p -> (p,calc_dist p pr1 pr2)) p_arr)
  in
  let scaled_shuff = 
    ArrayFuns.map2 
      (fun stat (p,l) -> (p, List.map (scale_dist stat) l))
      stats 
      shuff_results
  in
  (* the original data *)
  let dat_name = prefix^".dat" in
  let dat_ch = open_out dat_name in
  Array.iter 
    (fun (p,d) -> write_dist dat_ch p d)
    scaled_orig;
  close_out dat_ch;
  (* the shuffled data *)
  let shuff_dat_name = prefix^".shuffle.dat" in
  let dat_ch = open_out shuff_dat_name in
  Array.iter 
    (fun (p,l) -> List.iter (write_dist dat_ch p) l)
    scaled_shuff;
  close_out dat_ch;
  (* the r file *)
  let r_ch = open_out (prefix^".r") in
  Printf.fprintf r_ch "pdf(\"%s\")\n" (prefix^".pdf");
  Printf.fprintf r_ch "shuff_data <- read.table(\"%s\")\n" shuff_dat_name;
  Printf.fprintf r_ch "shuff_frame <- data.frame(p=shuff_data[,1],dist=shuff_data[,2])\n";
  Printf.fprintf r_ch "orig_data <- read.table(\"%s\")\n" dat_name;
  Printf.fprintf r_ch "low_bound <- function(v) { median(v) - 1.8 * IQR (v) }\n";
  Printf.fprintf r_ch "our_bounds <- c(low_bound(shuff_data[,2]),max(orig_data[,2]))\n";
  (* outline being false means do not draw outliers *)
  Printf.fprintf r_ch "boxplot(dist~p, data=shuff_frame, outline=FALSE, xlab=\"p\", ylab=\" rescaled Z_p distance\", ylim = our_bounds)\n";
  Printf.fprintf r_ch "points(orig_data[,2])\n";
  Printf.fprintf r_ch "dev.off()\n";
  close_out r_ch;
  ()



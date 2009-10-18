(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)


(* histogram shows where the sample sits in the shuffled distances *)
let write_histogram name1 name2 sample_dist shuffled_dists p =
  let prefix = "histo."^name1^".VS."^name2 in
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
  let n_samples = 11
  and min_p = 1e-2
  and max_p = 1e+2 in
  let p_arr = 
    Mokaphy_base.logarithmically_evenly_spaced 
      n_samples min_p max_p in
  let write_dist ch p x_pr1 x_pr2 = 
    Printf.fprintf ch "%g\t%g\n" p 
        (Placerun_distance.pair_dist
          criterion weighting p x_pr1 x_pr2)
  in
  (* the original data *)
  let dat_name = prefix^".dat" in
  let dat_ch = open_out dat_name in
  Array.iter 
    (fun p -> write_dist dat_ch p pr1 pr2)
    p_arr;
  close_out dat_ch;
  (* the shuffled data *)
  let shuff_dat_name = prefix^".shuffle.dat" in
  let dat_ch = open_out shuff_dat_name in
  Array.iter 
    (fun p -> 
      List.iter 
        (fun (spr1, spr2) ->
          write_dist dat_ch p spr1 spr2)
        shuffled_prs)
    p_arr;
  close_out dat_ch;
  (* the r file *)
  let r_ch = open_out (prefix^".r") in
  Printf.fprintf r_ch "pdf(\"%s\")\n" (prefix^".pdf");
  Printf.fprintf r_ch "shuff_data <- read.table(\"%s\")\n" shuff_dat_name;
  Printf.fprintf r_ch "shuff_frame <- data.frame(p=shuff_data[,1],dist=shuff_data[,2])\n";
  Printf.fprintf r_ch "orig_data <- read.table(\"%s\")\n" dat_name;
  Printf.fprintf r_ch "boxplot(dist~p, data=shuff_frame, main=\"Z_p distance\", xlab=\"p\")\n";
  Printf.fprintf r_ch "lines(orig_data[,2])\n";
  Printf.fprintf r_ch "dev.off()\n";
  close_out r_ch;
  ()



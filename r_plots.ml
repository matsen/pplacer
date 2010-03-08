(* mokaphy v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Fam_batteries
open MapsSets
open Multiset

(* the percent extra to stretch the x limits *)
let relax_factor = 0.05

let int_div x y = (float_of_int x) /. (float_of_int y)
let min_list l = ListFuns.complete_fold_left min l
let max_list l = ListFuns.complete_fold_left max l
let min_x all_dists = (1. -. relax_factor) *. (min_list all_dists)
let max_x all_dists = (1. +. relax_factor) *. (max_list all_dists)

(*
# R_plots.remove_from_list ~orig:[1.;2.;3.;4.;2.;2.;] ~to_remove:[2.; 2.; 1.];;
- : Multiset.FloatMultiset.M.key list = [2.; 3.; 4.]
*)
let remove_from_list ~orig ~to_remove = 
  FloatMultiset.to_list
    (List.fold_right 
      FloatMultiset.remove
      to_remove
      (FloatMultiset.of_list orig))

(* density shows where the sample sits in the shuffled distances *)
let write_density 
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
  let all_dists = sample_dist::shuffled_dists in
  Printf.fprintf r_ch 
    "plot(density(data[,1]), main=\"d(%s,%s) = %f\", xlab=\"KR Z_%g distance\",
    xlim=c(%g,%g))\n" 
    name1 name2 sample_dist p (min_x all_dists) (max_x all_dists);
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
        (Kr_distance.pair_distance criterion weighting p pr1 pr2))
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
    Kr_distance.pair_distance criterion weighting p x_pr1 x_pr2 in
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


let dists_between_ml_best_of_placerun placerun = 
  let (_, pl_map) = 
    Placerun.make_map_by_best_loc 
      Placement.ml_ratio
      placerun
  in
  Distance_mat.of_placement_list_map 
    (Placerun.get_ref_tree placerun) 
    (IntMap.map 
      (List.map (Pquery.best_place Placement.ml_ratio))
      pl_map)

(* ddensity shows the density of pairwise distances *)
let write_ddensity pr1 pr2 =
  let get_data pr = 
    let name = Placerun.get_name pr in
    let prefix = "ddensity."^name in
    (* the data *)
    (prefix, 
      Array.to_list
        (Uptri.get_data (dists_between_ml_best_of_placerun pr)))
  in
  let write_data prefix data = 
    let dat_name = prefix^".dat" in
    let dat_ch = open_out dat_name in
    List.iter 
      (fun x -> Printf.fprintf dat_ch "%g\n" x) 
      data;
    close_out dat_ch;
    (dat_name, data)
  in
  let gw_data pr = 
    let prefix, data = get_data pr in
    write_data prefix data
  in
  let prefix = 
    (Placerun.get_name pr1)^".and."^(Placerun.get_name pr2) in
  let dat_name1, data_l1 = gw_data pr1
  and dat_name2, data_l2 = gw_data pr2 in
  let ddensity_prefix, both_data = 
    get_data (Placerun.combine prefix pr1 pr2) in
  (* remove the within class distances from all pairwise distances *)
  let data_between = 
    remove_from_list ~to_remove:(data_l1 @ data_l2) ~orig:both_data
  in
  let dat_nameboth, _ = write_data ddensity_prefix data_between in
  (* the r file *)
  let r_ch = open_out (prefix^".r") in
  Printf.fprintf r_ch "pdf(\"%s\")\n" (prefix^".pdf");
  Printf.fprintf r_ch "data1 <- read.table(\"%s\")\n" dat_name1;
  Printf.fprintf r_ch "data2 <- read.table(\"%s\")\n" dat_name2;
  Printf.fprintf r_ch "databoth <- read.table(\"%s\")\n" dat_nameboth;
  Printf.fprintf r_ch 
    "plot(density(databoth[,1]), main=\"%s\", xlab=\"within tree distance\",xlim=c(0,%g))\n" 
    prefix 
    (max 
      (max (max_list data_l1) (max_list data_l2)) 
      (max_list data_between));
  Printf.fprintf r_ch "lines(density(data1[,1]),lty=2)\n";
  Printf.fprintf r_ch "lines(density(data2[,1]),lty=3)\n";
  Printf.fprintf r_ch "legend(\"topright\",c(\"%s\", \"%s\",\"%s\"),lty=c(1,2,3))\n" dat_nameboth dat_name1 dat_name2;
  Printf.fprintf r_ch "dev.off()\n";
  close_out r_ch;
  ()


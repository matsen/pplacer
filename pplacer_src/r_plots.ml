
open Ppatteries
(* open Multiset *)

(* the percent extra to stretch the x limits *)
let relax_factor = 0.05

let int_div x y = (float_of_int x) /. (float_of_int y)
let min_list l = List.reduce min l
let max_list l = List.reduce max l
let min_x all_dists = (1. -. relax_factor) *. (min_list all_dists)
let max_x all_dists = (1. +. relax_factor) *. (max_list all_dists)

(* density shows where the sample sits in the shuffled distances *)
let write_density p type_str name1 name2 sample_dist null_dists =
  (* the data *)
  let prefix = type_str^"."^name1^".VS."^name2 in
  let dat_name = prefix^".dat" in
  let dat_ch = open_out dat_name in
  List.iter
    (fun x -> Printf.fprintf dat_ch "%g\n" x)
    null_dists;
  close_out dat_ch;
  (* the r file *)
  let r_ch = open_out (prefix^".R") in
  Printf.fprintf r_ch "pdf(\"%s\")\n" (prefix^".pdf");
  Printf.fprintf r_ch "data <- read.table(\"%s\")\n" dat_name;
  let all_dists = sample_dist::null_dists in
  Printf.fprintf r_ch
  "plot(density(data[,1]), main=\"d(%s,%s) = %f\", xlab=expression(KR~Z[%g]~distance), xlim=c(%g,%g))\n"
    name1 name2 sample_dist p (min_x all_dists) (max_x all_dists);
  Printf.fprintf r_ch "abline(v=%g, col=\"red\")\n" sample_dist;
  Printf.fprintf r_ch "dev.off()\n";
  close_out r_ch;
  ()


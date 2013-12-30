open OUnit
open Test_util
open Linear_utils
open Ppatteries

let test_logdot _ =
  let m1 = Gsl_matrix.of_arrays [|[|-. 1.; 0.; 1.|]; [|1000.; 1.; 3.|]; [|0.; 0.; 2.|];|]
  and m2 = Gsl_matrix.of_arrays [|[|-. 1.; 1.; 2.|]; [|1.;    3.; 0.|]; [|0.; 1.; 2.|];|]
  in
  let mask_vec =
    Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout [|1; 0; 1;|]
  in
  "mat_masked_logdot" @? (log 3. +. log 4. =~ Linear.mat_masked_logdot m1 m2 mask_vec)

let suite = [
  "test_logdot" >:: test_logdot;
]

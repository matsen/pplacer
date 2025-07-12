open OUnit
open Ppatteries

let test_logdot _ =
  let mask_vec =
    Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout [|1; 0; 1;|]
  in
  (* Mat! *)
  let m1 = Gsl.Matrix.of_arrays [|[|-. 1.; 0.; 1.|]; [|1000.; 1.; 3.|]; [|0.; 0.; 2.|];|]
  and m2 = Gsl.Matrix.of_arrays [|[|-. 1.; 1.; 2.|]; [|1.;    3.; 0.|]; [|0.; 1.; 2.|];|]
  in
  "mat_masked_logdot" @? (log 3. +. log 4. =~ Linear.mat_masked_logdot m1 m2 mask_vec);
  (* Ten! *)
  let t1 = Tensor.of_arrays
            [|
                [|[|-. 1.; 0.; 1.|]; [|1000.; 1.; 3.|]; [|0.; 0.; 2.|];|];
                [|[|-. 1.; 0.; 1.|]; [|1000.; 1.; 3.|]; [|0.; 0.; 2.|];|];
            |]
  and t2 = Tensor.of_arrays
            [|
                [|[|-. 1.; 1.; 0.|]; [|1.;    3.; 0.|]; [|0.; 1.; 2.|];|];
                [|[|-. 1.; 1.; 2.|]; [|1.;    3.; 0.|]; [|0.; 1.; 3.|];|];
            |]
  in
  let ten_expected = (log (1. +. 3.) +. log (4. +. 6.)) -. 2. *. log 2. in
  let ten_util = Gsl.Vector.create 3 in
  "ten_masked_logdot" @?
    (ten_expected =~ Linear.ten_masked_logdot t1 t2 mask_vec ten_util);
  ()


let suite = [
  "test_logdot" >:: test_logdot;
]

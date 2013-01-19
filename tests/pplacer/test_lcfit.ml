open OUnit
open Test_util
open Ppatteries
open Lcfit

module BA = Bigarray
module BA1 = Bigarray.Array1

let m = {n00=1500.; n01=300.; n10=300.; n11=300.; r=1.; b=0.5; t=0.390296; rx=1.; bx=0.5}

let suite = [
  "test_matches_mathematica" >:: begin fun() ->
    let test_func (tx, c, l) =
      let actual = Lcfit.log_like c tx m in
      if not (approx_equal ~epsilon:1e-2 actual l) then
        assert_failure
          (Printf.sprintf
             "Log-likelihoods do not match: actual=%f expected=%f"
             actual
             l)
    in
    List.iter
      test_func
      (* (tx, c, log-likelihood) *)
      [(0.01, 0.1, -4371.24);
       (0.01, 0.2, -4370.45);
       (0.01, 0.3, -4371.41);
       (0.1, 0.1, -4390.43);
       (0.1, 0.2, -4389.98);
       (0.1, 0.3, -4390.53);
       (1.0, 0.1, -4574.57);
       (1.0, 0.2, -4575.06);
       (1.0, 0.3, -4574.47);
      ];
  end;
  "test_fit_success" >:: begin fun() ->
    let of_array = BA1.of_array BA.float64 BA.c_layout in
    let c = of_array [|0.350001; 0.310001; 0.280001; 0.320001; 0.330001; 0.180001; 0.170001; 0.270001; 0.140001; 0.380001; 0.250001; 0.220001; 0.170001; 0.160001; 0.020001; 0.360001; 0.210001; 0.270001; 0.240001; 0.370001|]
    and tx = of_array [|0.460001; 0.140001; 0.600001; 0.020001; 0.260001; 0.400001; 0.170001; 0.090001; 0.050001; 0.070001; 0.120001; 0.790001; 0.330001; 0.660001; 0.190001; 0.400001; 0.500001; 0.200001; 0.390001; 0.120001|]
    and l = of_array [|-700.233911883; -721.128947905; -697.193926396; -793.03382342; -706.334497659; -694.312881641; -708.884610045; -730.369797453; -750.584426125; -777.020173121; -719.268957126; -702.183981614; -695.897769298; -698.511693664; -733.643533379; -702.732261827; -694.35172057; -706.058159612; -694.333672064; -744.396900924; |]
    and scaled = Lcfit.rescale (0.180001, 0.400001, -694.3129) m in
    let fit_model = Lcfit.fit c tx l scaled in
    let err = 0--((BA1.dim c) - 1)
      |> map (fun i -> (c.{i},tx.{i},l.{i}))
      |> map (fun (c, tx, l) -> l -. (Lcfit.log_like c tx fit_model) |> abs_float)
      |> reduce (+.)
    in
    (* TODO: better fit test - this just checks for success. *)
    assert_bool (Printf.sprintf "Error out of range: %f" err) (err < 5.);
  end;
]

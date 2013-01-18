open OUnit
open Test_util
open Ppatteries
open Lcfit

let suite = [
  "test_matches_mathematica" >:: begin fun() ->
    let m =
      {n00=1500.; n01=300.; n10=300.; n11=300.; r=1.; b=0.5; t=0.390296; rx=1.; bx=0.5}
    in
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
      (* (c, tx, log-likelihood) *)
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
]

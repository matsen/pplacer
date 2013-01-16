open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_median" >:: begin fun () ->
    List.iter
      (Tuple.Tuple2.map1 median |- uncurry assert_equal)
      [
        [0], 0;
        [0; 1], 0;
        [0; 1; 2], 1;
        [0; 1; 2; 3], 1;
        [0; 1; 2; 3; 4], 2;
      ];
    assert_raises (Invalid_argument "median") (fun () -> median []);

  end;
]

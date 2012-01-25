open Ppatteries
open OUnit
open Test_util

let test exponent results () =
  placeruns_of_dir "simple"
    |> List.cons (placerun_of_dir "multi" "test1and3")
    |> List.map
        (Placerun.get_name &&& Guppy_fpd.wpd_of_placerun Placement.ml_ratio exponent)
    |> List.sort compare
    |> List.enum
    |> check_map_approx_equal
        "unequal (%s(%g) and %s(%g))"
        (List.enum results)

let suite = [
  "test_simple_1" >::
    test 1. [
            "test1", 8.;
            "test1and3", 6.66667;
            "test2", 4.;
            "test3", 0.;
    ];
  "test_simple_0.5" >::
    test 0.5 [
            "test1", 8.;
            "test1and3", 10. *. (sqrt (2./.3.));
            "test2", 4.;
            "test3", 0.;
    ];
]

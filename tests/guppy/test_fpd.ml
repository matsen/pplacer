open Ppatteries
open OUnit
open Test_util

let test ?include_pendant exponent results () =
  placeruns_of_dir "simple"
    |> List.cons (placerun_of_dir "multi" "test1and3")
    |> List.map
        (Placerun.get_name
         &&& Guppy_fpd.awpd_of_placerun ?include_pendant Placement.ml_ratio exponent)
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
  "test_pendant_1" >::
    test ~include_pendant:true 1. [
      "test1", 28.;
      "test1and3", 26.66667;
      "test2", 24.;
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

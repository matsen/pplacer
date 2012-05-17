open Ppatteries
open OUnit
open Test_util

let test ?include_pendant results () =
  placeruns_of_dir "simple"
    |> List.cons (placerun_of_dir "multi" "test1and3")
    |> List.map
        (Placerun.get_name
         &&& Guppy_fpd.pd_of_placerun ?include_pendant Placement.ml_ratio)
    |> List.sort compare
    |> List.enum
    |> check_map_approx_equal
        "unequal (%s(%g) and %s(%g))"
        (List.enum results)

let suite = [
  "test_simple" >::
    test [
      "test1", 8.;
      "test1and3", 10.;
      "test2", 4.;
      "test3", 0.;
    ];
  "test_pendant" >::
    test ~include_pendant:true [
      "test1", 28.;
      "test1and3", 40.;
      "test2", 24.;
      "test3", 0.;
    ];
]

open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_simple" >:: begin fun () ->
    placerun_of_dir "simple" "test1"
      |> Rarefaction.of_placerun Placement.ml_ratio
      |> check_map_approx_equal
          "unequal (%d(%g) and %d(%g))"
          (List.enum [
            2, 8.;
          ])
  end;

  "test_multi" >:: begin fun () ->
    placerun_of_dir "multi" "test1and3"
      |> Rarefaction.of_placerun Placement.ml_ratio
      |> check_map_approx_equal
          "unequal (%d(%g) and %d(%g))"
          (List.enum [
            2, 6.66667;
            3, 10.;
          ])
  end;

]

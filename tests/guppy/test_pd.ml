open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_simple" >:: begin fun () ->
    placeruns_of_dir "simple"
      |> List.map
          (Placerun.get_name &&& Guppy_pd.pd_of_placerun Placement.ml_ratio false)
      |> List.sort compare
      |> List.enum
      |> check_map_approx_equal
          "unequal (%s(%g) and %s(%g))"
          (List.enum [
            "test1", 8.;
            "test2", 4.;
            "test3", 0.;
          ])
  end;

]

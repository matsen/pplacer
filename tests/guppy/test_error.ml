open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_error" >:: begin fun () ->
    let exper = placerun_of_dir "misc" "test_error_experimental"
    and exper_scaled = placerun_of_dir "misc" "test_scaled_error_experimental"
    and expec = placerun_of_dir "misc" "test_error_expected" in
    Guppy_error.pr_error Placement.ml_ratio false false exper expec
    |> StringMap.enum
    |> check_map_approx_equal
        "unequal (%s(%g) and %s(%g))"
        (List.enum [
          "one", 0.34;
          "three", 1.;
          "two", 0.94;
        ]);
    Guppy_error.pr_error Placement.ml_ratio true false exper expec
    |> StringMap.enum
    |> check_map_approx_equal
        "unequal (%s(%g) and %s(%g))"
        (List.enum [
          "one", 18.34;
          "three", 16.;
          "two", 17.94;
        ]);
    Guppy_error.pr_error Placement.ml_ratio false true exper_scaled expec
    |> StringMap.enum
    |> check_map_approx_equal
        "unequal (%s(%g) and %s(%g))"
        (List.enum [
          "one", 0.34;
          "three", 1.;
          "two", 0.94;
        ]);
  end;

]

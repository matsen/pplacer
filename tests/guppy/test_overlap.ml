open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_overlap" >:: begin fun () ->
    placerun_of_dir "misc" "test_islands"
      |> Placerun.get_pqueries
      |> Mass_overlap.of_pql Placement.ml_ratio
      |> List.of_enum
      |> List.sort compare
      |> List.iter2
          (fun (s1a, s1b, v1) (s2a, s2b, v2) ->
            "not equal" @? (s1a = s2a && s1b = s2b && v1 =~ v2))
          [
            "four", "five", 0.12;
            "three", "one", 0.09;
            "two", "three", 0.04;
          ]
  end;

]

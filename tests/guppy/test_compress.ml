open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_compress" >:: begin fun () ->
    placerun_of_dir "misc" "test_compress"
      |> Mass_compress.of_placerun
          Mass_islands.of_pql
          ~c:1.75
          Mass_map.Spread
          Placement.ml_ratio
      |> List.for_all2
          pquery_equal
          (placerun_of_dir "misc" "compressed" |> Placerun.get_pqueries)
      |> (@?) "not equal"
  end;

]

open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_compress" >:: begin fun () ->
    placerun_of_dir "misc" "test_compress"
      |> Mass_compress.of_placerun ~c:1.75 Mass_map.Weighted Placement.ml_ratio
      |> (=) (placerun_of_dir "misc" "compressed" |> Placerun.get_pqueries)
      |> (@?) "not equal"
  end;

]

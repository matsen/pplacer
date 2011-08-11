open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_simple" >:: begin fun () ->
    let indiv_of = Mass_map.Indiv.of_placerun
      Mass_map.unit_transform
      Mass_map.Weighted
      Placement.ml_ratio
    in
    placeruns_of_dir "simple"
      |> List.cons
          (placeruns_of_dir "multi"
           |> List.find (Placerun.get_name |- (=) "test1and3"))
      |> List.map
          (Placerun.get_name &&& Guppy_wpd.wpd_of_placerun indiv_of)
      |> List.sort
      |> List.enum
      |> check_map_approx_equal
          "unequal (%s and %s)"
          (List.enum [
            "test1", 8.;
            "test1and3", 6.66667;
            "test2", 4.;
            "test3", 0.;
          ])
  end;

]

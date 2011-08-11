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
      |> List.map
          (Placerun.get_name &&& Guppy_pd.pd_of_placerun indiv_of false)
      |> List.sort
      |> List.enum
      |> check_map_approx_equal
          "unequal (%s and %s)"
          (List.enum [
            "test1", 8.;
            "test2", 4.;
            "test3", 0.;
          ])
  end;

]

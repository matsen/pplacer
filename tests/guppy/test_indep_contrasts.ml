open Ppatteries
open OUnit
open Test_util

let leaf_values = StringMap.of_pairlist ["A", 1.; "B", 2.; "C", 3.; "D", 4.]
let holed_leaf_values = StringMap.of_pairlist ["A", 3.; "D", 1.]

let test_of_values_and_expected values expected () =
  placerun_of_dir "misc" "test_indep_contrasts"
    |> Indep_contrasts.of_criterion_map Placement.ml_ratio values
    |> List.map
        (fun (pq, x) -> List.map (curry identity x) (Pquery.namel pq))
    |> List.flatten
    |> List.iter2
        (fun (en, ex) (gx, gn) ->
          assert_equal ~cmp:approx_equal ex gx;
          assert_equal en gn)
        expected

let suite = [
  "test_of_criterion_map" >:: begin
    test_of_values_and_expected
      leaf_values
      [
        "A", 1.32;
        "B", 1.92;
        "C", 3.48;
        "D", 3.;
        "E", 1.64;
        "F", 2.2;
        "G", 2.2;
        "H", 2.2;
        "I", 2.06;
        "J", 1.857;
      ]
  end;

  "test_of_criterion_map_with_holes" >:: begin
    test_of_values_and_expected
      holed_leaf_values
      [
        "A", 2.8;
        "B", 2.6;
        "C", 1.2;
        "D", 1.2;
        "E", 2.6;
        "F", 2.2;
        "G", 2.2;
        "H", 2.2;
        "I", 2.3;
        "J", 2.445;
      ]
  end;

  "test_scale_placerun" >:: begin fun () ->
    let printer = Pquery_io.to_json (ref None)
      |- Json.to_string
    in
    placerun_of_dir "misc" "test_indep_contrasts"
      |> Indep_contrasts.scale_placerun Placement.ml_ratio leaf_values
      |> Placerun.get_pqueries
      |> List.iter2
          (assert_equal ~cmp:pquery_equal ~printer)
          (placerun_of_dir "misc" "indep_scaled" |> Placerun.get_pqueries)
  end;

]

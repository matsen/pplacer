open Ppatteries
open OUnit
open Test_util

let leaf_values = StringMap.of_pairlist ["A", 1.; "B", 2.; "C", 3.; "D", 4.]

let suite = [
  "test_of_criterion_map" >:: begin fun () ->
    placerun_of_dir "misc" "test_indep_contrasts"
      |> Indep_contrasts.of_criterion_map Placement.ml_ratio leaf_values
      |> List.map
          (fun (pq, x) -> List.map (curry identity x) (Pquery.namel pq))
      |> List.flatten
      |> List.iter2
          (fun (ex, en) (gx, gn) ->
            assert_equal ~cmp:approx_equal ex gx;
            assert_equal en gn)
          [
            1.32, "A";
            1.92, "B";
            3.48, "C";
            3., "D";
            1.64, "E";
            2.2, "F";
            2.2, "G";
            2.2, "H";
            2.06, "I";
            1.857, "J";
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

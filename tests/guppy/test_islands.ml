open Ppatteries
open OUnit
open Test_util

let reduce_and_check expected =
  List.sort (on fst IntSet.compare)
  |- List.map
      (Tuple.Tuple2.map2 (List.map Pquery.namel |- List.flatten |- StringSet.of_list))
  |- List.iter2
      (fun (is_e, ss_e) (is_a, ss_a) ->
        "not equal"
        @? (IntSet.equal is_e is_a && StringSet.equal ss_e ss_a))
      (List.map (Tuple2.map IntSet.of_list StringSet.of_list) expected)

let suite = [
  "test_islands" >:: begin fun () ->
    placerun_of_dir "misc" "test_islands"
      |> Placerun.get_pqueries
      |> Mass_islands.of_pql (const 0.)
      |> reduce_and_check
          [
            [0; 1; 2], ["one"; "two"; "three"];
            [3; 4], ["four"; "five"];
          ]
  end;

  "test_discard_below" >:: begin fun () ->
    placerun_of_dir "misc" "test_islands"
      |> Placerun.get_pqueries
      |> Mass_islands.of_pql
          ~discard_below:0.25
          Placement.ml_ratio
      |> reduce_and_check
          [
            [0; 2], ["one"; "three"];
            [3], ["four"];
            [4], ["five"];
          ]
  end;

]

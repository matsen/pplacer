open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_islands" >:: begin fun () ->
    placerun_of_dir "misc" "test_islands"
      |> Placerun.get_pqueries
      |> Mass_islands.of_pql
      |> List.sort ~cmp:(on fst IntSet.compare)
      |> List.map
          (second (List.map Pquery.namel |- List.flatten |- StringSet.of_list))
      |> List.iter2
          (fun (is_e, ss_e) (is_a, ss_a) ->
            "not equal"
            @? (IntSet.equal is_e is_a && StringSet.equal ss_e ss_a))
          (List.map
             (IntSet.of_list *** StringSet.of_list)
             [
               [0; 1; 2], ["one"; "two"; "three"];
               [3; 4], ["four"; "five"];
             ])
  end;

]

open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_rarefy" >:: begin fun () ->
    let sample ~p =
      assert_equal p [|1.; 1.; 2.; 3.; 0.5; 0.25; 1.; 1.; 1.|];
      [|1; 0; 0; 0; 1; 0; 2; 1; 0|]
    in
    placerun_of_dir "misc" "test_rarefy"
      |> Placerun.get_pqueries
      |> Array.of_list
      |> Guppy_rarefy.rarefy sample
      |> List.map (Pquery.namlom |- List.sort (comparing fst))
      |> List.sort (List.compare (comparing fst))
      |> assert_equal [["A", 1.]; ["E", 1.]; ["G", 2.; "H", 1.]]

  end;

]

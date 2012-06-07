open Ppatteries
open OUnit
open Test_util

let check_triples =
  Enum.iter2
    (fun (a1, b1, c1) (a2, b2, c2) ->
      (Printf.sprintf "unequal (%d(%g, %g) and %d(%g, %g))"
         a1 b1 c1 a2 b2 c2)
      @? (a1 = a2 && b1 =~ b2 && c1 =~ c2))

let suite = [
  "test_simple" >:: begin fun () ->
    placerun_of_dir "simple" "test1"
      |> Rarefaction.of_placerun Placement.ml_ratio
      |> check_triples
          (List.enum [
            2, 8., 15.;
          ])
  end;

  "test_multi" >:: begin fun () ->
    placerun_of_dir "multi" "test1and3"
      |> Rarefaction.of_placerun Placement.ml_ratio
      |> check_triples
          (List.enum [
            2, 6.66667, 12.33333;
            3, 10., 15.;
          ])
  end;

  "test_hand_mean" >:: begin fun () ->
    placerun_of_dir "misc" "test_rarefaction"
      |> Rarefaction.of_placerun Placement.ml_ratio
      |> check_triples
          (List.enum [
            2, 4.66667, 5.;
            3, 7., 7.;
          ])
  end;

  "test_hand_variance" >:: begin fun () ->
    placerun_of_dir "misc" "test_rarefaction"
      |> Rarefaction.variance_of_placerun Placement.ml_ratio
      |> check_triples
          (List.enum [
            2, 1.55556, 0.66667;
            3, 0., 0.;
          ])
  end;

]

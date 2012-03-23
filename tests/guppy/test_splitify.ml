open Ppatteries
open OUnit
open Test_util

let fal = [
  [| -1.; -1.; -1.; -1.; -1.; -1.; -1.; -1.;
     -0.777777777; -0.333333333; -1.; -1.; 0.333333333; 1. |];
  [| -1.; -1.; -1.; -1.; -1.; -1.; -1.; -1.;
     -0.777777777; -0.555555555; -0.777777777; -0.777777777; -0.111111111; 1. |];
  [| -1.; -1.; -1.; -1.; -1.; -1.; -1.; -1.;
     -0.75; -1.; 0.; -1.; 0.5; 1. |];
]

let gt = Newick_gtree.of_string
  "(((C:2{2},D:2{3}):2{9},(E:2{4},F:2{5}):2{10}):2{12},(A:2{0},B:2{1}):2{8},(G:2{6},H:2{7}):2{11}):0{13};"

let suite = [
  "test_rep_edges" >:: begin fun () ->
    let printer s =
      IO.output_string () |> tap (flip (IntSet.print Int.print) s) |> IO.close_out
    in
    List.fold_left
      (fun prev_edges (max_edge_d, edge_changes) ->
        let expected_edges = IntSet.of_list edge_changes
          |> IntSet.sdiff prev_edges
        and got_edges = Guppy_splitify.find_rep_edges max_edge_d fal gt in
        assert_equal ~printer ~cmp:IntSet.equal ~msg:(Printf.sprintf "%g" max_edge_d) expected_edges got_edges;
        expected_edges)
      (0 -- 13 |> IntSet.of_enum)
      (* the second value represents the edges added or removed at each step *)
      [
        0.1, [];
        0.3, [11];
        0.5, [8];
        0.9, [9];
        1.5, [10; 13];
        3.0, [12; 13];
        4.0, [13];
      ]
    |> ignore
  end;

]

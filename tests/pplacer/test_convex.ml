open OUnit
open Test_util

open Convex
open MapsSets

let suite = List.map
  (fun (s, omega) ->
    let gt = Newick_gtree.of_string s in
    let st = gt.Gtree.stree
    and bm = gt.Gtree.bark_map in
    let colors = IntMap.map (fun v -> v#get_name) bm in
    let _, calculated_omega = solve (colors, st) in
    let testfunc () =
      (Printf.sprintf "%d expected; got %d" omega calculated_omega) @? (omega = calculated_omega)
    in
    s >:: testfunc)
  [
    "((A,A),(B,B))", 4;
    "((A,B),(A,B))", 3;
    "(((A,B),(A,B)),(C,C))", 5;
    "(((A,B),B),(A,A))", 4;
    "(A,(A,(B,C)))", 4;
  ]

let suite =
  ("alternate_colors" >::
      fun () ->
        let gt = Newick_gtree.of_string "(A,(A,(X,(B,B))))" in
        let st = gt.Gtree.stree
        and bm = gt.Gtree.bark_map in
        let colors = IntMap.remove 2 (IntMap.map (fun v -> v#get_name) bm) in
        let alt_colors = alternate_colors (colors, st) in
        assert_equal (IntMap.nkeys alt_colors) 1;
        assert_equal (IntMap.find 2 alt_colors) (ColorSet.of_list ["A"; "B"])
  ) :: suite


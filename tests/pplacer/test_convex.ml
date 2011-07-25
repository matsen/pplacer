open OUnit
open Test_util

open Convex
open MapsSets

let suite = List.map
  (fun (s, omega) ->
    let gt = Newick_gtree.of_string s in
    let st = gt.Gtree.stree
    and bm = gt.Gtree.bark_map in
    let colors = MapsSets.IntMap.fold
      (fun key value map ->
        try
          let name = value#get_name in
          MapsSets.IntMap.add key name map
        with Newick_bark.No_name -> map)
      bm
      MapsSets.IntMap.empty
    in
    let naive_nodes = Naive.solve (colors, st) in
    let _, early_omega = solve ~nu_f:apart_nu (colors, st)
    and _, not_early_omega = solve ?nu_f:None (colors, st)
    and naive_omega = IntSet.cardinal naive_nodes in
    let testfunc () =
      (Printf.sprintf "%d expected; got %d with early termination" omega early_omega) @? (omega = early_omega);
      (Printf.sprintf "%d expected; got %d without early termination" omega not_early_omega) @? (omega = not_early_omega);
      (Printf.sprintf "%d expected; got %d naively" omega naive_omega) @? (omega = naive_omega);
    in
    s >:: testfunc)
  [
    "((A,A),(B,B))", 4;
    "((A,B),(A,B))", 3;
    "(((A,B),(A,B)),(C,C))", 5;
    "(((A,B),B),(A,A))", 4;
    "(A,(A,(B,C)))", 4;
    "(A,((A,(B,A)),B))", 4;
    "(A,((A,B),(A,B)));", 4;
    "(A,(B,(A,(B,A))));", 4;
    "(A,(A,(B,(C,(C,(A,C))))));", 6;
    "(A,(A,(B,(C,((A,C),(B,C))))));", 6;
  ]

let suite =
  ("alternate_colors" >::
      fun () ->
        let gt = Newick_gtree.of_string "(A,(A,(X,(B,(X,B)))))" in
        let st = gt.Gtree.stree
        and bm = gt.Gtree.bark_map in
        let colors = IntMap.filter
          (fun _ v -> v <> "X")
          (IntMap.map (fun v -> v#get_name) bm)
        in
        let alt_colors = alternate_colors (colors, st) in
        assert_equal (IntMap.nkeys alt_colors) 2;
        "first color sets not equal" @?
          (ColorSet.equal
             (IntMap.find 2 alt_colors)
             (ColorSet.of_list ["A"; "B"]));
        "second color sets not equal" @?
          (ColorSet.equal
             (IntMap.find 4 alt_colors)
             (ColorSet.of_list ["B"]))
  ) :: (
    "building_cutsets" >::
      fun () ->
        let gt = Newick_gtree.of_string "(A,(A,(B,(C,C))))" in
        let st = gt.Gtree.stree
        and bm = gt.Gtree.bark_map in
        let colors = IntMap.map (fun v -> v#get_name) bm in
        let _, cutset = build_sizemim_and_cutsetim (colors, st) in
        let expected = IntMap.map
          ColorSet.of_list
          (IntMap.of_pairlist
             [
               0, ["A"];
               1, ["A"];
               2, [];
               3, ["C"];
               4, ["C"];
               5, [];
               6, [];
               7, ["A"];
               8, ["A"; "B"; "C"];
             ])
        in
        assert_equal (IntMap.compare ColorSet.compare cutset expected) 0
  ) :: suite


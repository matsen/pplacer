open OUnit
open Test_util

open Convex
open Ppatteries

let convex_suite = List.map
  (fun (s, omega) ->
    let gt = Newick_gtree.of_string s in
    let st = gt.Gtree.stree
    and bm = gt.Gtree.bark_map in
    let colors = IntMap.filter_map
      (fun _ value -> value#get_node_label_opt |> Option.map Tax_id.of_string)
      bm
    in
    let extra, st' = prune_tree (colors, st) in
    let naive_nodes = Naive.solve (colors, st) in
    let _, early_omega = solve ~nu_f:apart_nu (colors, st')
    and _, not_early_omega = solve ?nu_f:None (colors, st')
    and naive_omega = IntSet.cardinal naive_nodes
    and extra_omega = IntSet.cardinal extra in
    let early_omega = extra_omega + early_omega
    and not_early_omega = extra_omega + not_early_omega in
    let testfunc () =
      (Printf.sprintf "%d expected; got %d with early termination" omega early_omega) @? (omega = early_omega);
      (Printf.sprintf "%d expected; got %d without early termination" omega not_early_omega) @? (omega = not_early_omega);
      (* just testing naive with bifurcation for now *)
      if Stree.outdegree st' <= 2 then
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
    "((((((((A,B),B),C),),(,)),(,((D,C),),)),((A,A),)),E);", 15;
    "(A,A,B,B);", 4;
    "(A,A,A,B,B);", 5;
    "((A,B),A,B,(A,B));", 4;
    "(A,((A,B),A,B,(A,B)));", 5;
  ]

let strict_suite = List.map
  (fun (s, omega) ->
    let gt = Newick_gtree.of_string s in
    let st = gt.Gtree.stree
    and bm = gt.Gtree.bark_map in
    let colors = IntMap.filter_map
      (fun _ value -> value#get_node_label_opt |> Option.map Tax_id.of_string)
      bm
    in
    let _, early_omega = solve ~strict:true ~nu_f:apart_nu (colors, st)
    and _, not_early_omega = solve ~strict:true ?nu_f:None (colors, st) in
    let testfunc () =
      (Printf.sprintf "%d expected; got %d with early termination" omega early_omega) @? (omega = early_omega);
      (Printf.sprintf "%d expected; got %d without early termination" omega not_early_omega) @? (omega = not_early_omega);
    in
    s >:: testfunc)
  [
    "((A,A),(B,B))", 4;
    "((A,B),(A,B))", 2;
    "((A,(A,B)),(B,(B,A)))", 4;
    "(((A,B),(A,B)),(C,C))", 4;
    "(A,(A,(B,C)))", 3;
    "(A,A,B)", 3;
    "(A,A,B,B)", 4;
    "(A,A,A,B,B)", 5;
    "(A,A,(A,B),B)", 4;
    "((A,B),(A,B),(A,B))", 3;
  ]

let suite = [
  "alternate_colors" >:: begin fun () ->
    let gt = Newick_gtree.of_string "(A,(A,(X,(B,(X,B)))))" in
    let st = gt.Gtree.stree
    and bm = gt.Gtree.bark_map in
    let colors = IntMap.filter_map
      (fun _ value ->
        value#get_node_label_opt
          |> (flip Option.bind
              (function "X" -> None | x -> Some (Tax_id.of_string x))))
      bm
    in
    let alt_colors = alternate_colors (colors, st) in
    assert_equal (IntMap.cardinal alt_colors) 2;
    "first color sets not equal" @?
      (ColorSet.equal
         (IntMap.find 2 alt_colors)
         (colorset_of_strings ["A"; "B"]));
    "second color sets not equal" @?
      (ColorSet.equal
         (IntMap.find 4 alt_colors)
         (colorset_of_strings ["B"]))
  end;
  "building_cutsets" >:: begin fun () ->
    let gt = Newick_gtree.of_string "(A,(A,(B,(C,C))))" in
    let st = gt.Gtree.stree
    and bm = gt.Gtree.bark_map in
    let colors = IntMap.filter_map
      (fun _ value -> value#get_node_label_opt |> Option.map Tax_id.of_string)
      bm
    in
    let _, cutset = build_sizemim_and_cutsetim (colors, st) in
    let expected = IntMap.map
      colorset_of_strings
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
  end;
  "convexify" >::: convex_suite;
  "strict_convexify" >::: strict_suite;
]

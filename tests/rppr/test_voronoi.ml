open OUnit
open Test_util
open Voronoi
open MapsSets

let test_suite_of_gtree_and_expected (gt_string, distr) =
  let gt = Newick_gtree.of_string gt_string in
  let v = of_gtree gt in
  let ldistm' = IntMap.filter
    (fun k _ -> not (IntSet.mem k v.all_leaves))
    v.ldistm
  in
  let ldistl =
    List.map
      (fun (leaf, ld) -> leaf, ld.leaf, ld.distance)
      (IntMap.to_pairs ldistm')
  in
  gt_string >::: List.map2
    (fun got expected ->
      let node, _, _ = expected in
      (Printf.sprintf "internal node %d" node)
      >:: fun () ->
        "distance or node not equal" @? match got, expected with
          | (n1, lf1, d1), (n2, Some lf2, d2) ->
            n1 = n2 && lf1 = lf2 && approx_equal d1 d2
          | (n1, _, d1), (n2, None, d2) ->
            n1 = n2 && approx_equal d1 d2)
    ldistl
    distr

let test_v = of_gtree
  (Newick_gtree.of_string "(x:.3,(x:.1,(x:.4,x:.5):.1):.4)")

let snipl_equal l1 l2 =
  List.for_all2
    (fun (l1, s1, f1) (l2, s2, f2) ->
      l1 = l2 && approx_equal s1 s2 && approx_equal f1 f2)
    l1
    l2

let suite = [
  "test_uncoloring" >:: begin fun () ->
    List.iter
      (fun l ->
        (Printf.sprintf
           "uncoloring with _leaves and _leaf didn't match (leaf %d)"
           l)
        @? ((uncolor_leaves test_v (IntSet.singleton l))
            = (uncolor_leaf test_v l)))
      [0; 1; 2; 3];
    List.iter
      (fun (leaves, expected_updated) ->
        let _, got_updated = uncolor_leaves test_v (IntSet.of_list leaves) in
        "unexpected updated leaves when uncoloring"
        @? (IntSet.equal got_updated (IntSet.of_list expected_updated)))
      [
        [0], [1];
        [1], [0; 2];
        [2], [1];
        [3], [1];
        [2; 3], [1];
        [0; 1], [2];
      ]
  end;

  "test_snips" >:: begin fun () ->
    let expected = [
        0, [
          0, 0.3, 0.0;
          5, 0.4, 0.3;
        ];
        1, [
          1, 0.1, 0.0;
          2, 0.4, 0.3;
          3, 0.5, 0.35;
          4, 0.1, 0.0;
          5, 0.3, 0.0;
        ];
        2, [
          2, 0.3, 0.0;
        ];
        3, [
          3, 0.35, 0.0;
        ];
        4, [];
      ]
    in
    List.iter
      (fun (leaf, expected_snips) ->
        let got_snips = List.sort compare (get_edge_snipl test_v leaf) in
        (Printf.sprintf "unexpected snipl for leaf %d" leaf)
        @? (snipl_equal got_snips expected_snips))
      expected
  end;

  "test_mass_distribution" >:: begin fun () ->
    assert_raises
      Not_found
      (fun () -> distribute_mass test_v (IntMap.singleton 4 [0.5, 0.0]));
    let got_massdist = distribute_mass
      test_v
      (IntMap.of_pairlist [
        3, [0.0, 1.0; 0.25, 2.0; 0.35, 3.0; 0.4, 4.0];
        5, [0.0, 5.0; 0.3, 6.0; 0.35, 7.0];
        4, [0.0, 8.0; 0.05, 9.0];
        2, [0.0, 10.0; 0.3, 11.0; 0.35, 12.0];
      ])
    in
    List.iter
      (fun (leaf, masslist) ->
        (Printf.sprintf "unexpected mass distribution for leaf %d" leaf) @?
          (List.for_all2
             approx_equal
             masslist
             (List.sort compare (IntMap.find leaf got_massdist))))
      [
        0, [7.];
        1, [4.; 5.; 6.; 8.; 9.; 12.];
        2, [10.; 11.];
        3, [1.; 2.; 3.];
      ]
  end;
]

let suite = suite @
  List.map
    test_suite_of_gtree_and_expected
    [
      "(x:.3,(x:.1,(x:.4,x:.5):.1):.4)", [
        4, Some 1, 0.2;
        5, Some 1, 0.1;
        6, Some 0, 0.3;
      ];
      "((x:.25,x:.25):.25,(x:.25,x:.25):.25)", [
        2, None, 0.25;
        5, None, 0.25;
        6, None, 0.5;
      ];
    ]

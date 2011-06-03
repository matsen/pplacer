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

let suite =
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

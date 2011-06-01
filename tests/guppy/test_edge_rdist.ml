open OUnit
open Edge_rdist

let test_rt = Newick_gtree.of_string "((A:2,B:9):7,C:5,D:1):0;"
let dm = build_pairwise_dist test_rt

let test_pairwise correct e1 d1 e2 d2 =
  (Printf.sprintf "%f <> d (%d,%f) (%d,%f)" correct e1 d1 e2 d2) @?
    (correct = find_pairwise_dist dm e1 d1 e2 d2)

let suite = [
  "pairwise_dist" >::: [
    "same edge" >:: (fun _ -> test_pairwise 5. 1 2. 1 7.);
    "serial edge above first" >:: (fun _ -> test_pairwise 4. 2 2. 1 7.);
    "serial edge below first" >:: (fun _ -> test_pairwise 4. 1 7. 2 2.);
    "parallel edges" >:: (fun _ -> test_pairwise 4.5 0 0.5 1 6.);
  ]
]


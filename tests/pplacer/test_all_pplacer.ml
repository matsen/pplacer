open OUnit

let suite = [
  "edge_labels" >::: Test_edge_labels.suite;
  "matrix" >::: Test_matrix.suite;
  "linear" >::: Test_linear.suite;
  (* "diagd" >::: Test_diagd.suite; *)
  "placefile" >::: Test_placefile.suite;
  "convex" >::: Test_convex.suite;
  "like" >::: Test_like.suite;
  "rerooting" >::: Test_rerooting.suite;
  "exp_priors" >::: Test_exp_priors.suite;
  "painting" >::: Test_painting.suite;
  "newick_parser" >::: Test_newick_parser.suite;
  "ppatteries" >::: Test_ppatteries.suite;
  "misc" >::: Test_misc.suite;
]

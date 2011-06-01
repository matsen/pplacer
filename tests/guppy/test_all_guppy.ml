open OUnit

let suite = [
  "kr_distance" >::: Test_kr_distance.suite;
  "power_iteration" >::: Test_power_iteration.suite;
  "pca" >::: Test_pca.suite;
  "heat" >::: Test_heat.suite;
  "edge_rdist" >::: Test_edge_rdist.suite;
]

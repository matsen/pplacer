open OUnit

let suite = [
  "kr_distance" >::: Test_kr_distance.suite;
  "power_iteration" >::: Test_power_iteration.suite;
  "pca" >::: Test_pca.suite;
  "heat" >::: Test_heat.suite;
  "edge_rdist" >::: Test_edge_rdist.suite;
  "edpl" >::: Test_edpl.suite;
  "gaussian" >::: Test_gaussian.suite;
  "pd" >::: Test_pd.suite;
  "wpd" >::: Test_wpd.suite;
  "rarefact" >::: Test_rarefact.suite;
  "islands" >::: Test_islands.suite;
  "compress" >::: Test_compress.suite;
]

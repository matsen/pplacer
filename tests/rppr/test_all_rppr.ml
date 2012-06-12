open OUnit

let suite = [
  "voronoi" >::: Test_voronoi.suite;
  "convex_taxids" >::: Test_convex_taxids.suite;
]

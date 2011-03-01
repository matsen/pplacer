open OUnit

let suite = [
  "kr_distance" >::: Test_kr_distance.suite;
  "pca" >::: Test_pca.suite;
]

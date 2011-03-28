open OUnit

let suite = "all tests" >::: [
  "guppy" >::: Test_all_guppy.suite;
  "pplacer" >::: Test_all_pplacer.suite;
  "json" >::: Test_json.suite;
]

let _ =
  run_test_tt_main suite


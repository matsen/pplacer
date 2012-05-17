open Ppatteries
open OUnit

let suite = "all tests" >::: [
  "guppy" >::: Test_all_guppy.suite;
  "pplacer" >::: Test_all_pplacer.suite;
  "rppr" >::: Test_all_rppr.suite;
  "json" >::: Test_json.suite;
]

let _ =
  verbosity := 0;
  run_test_tt_main suite


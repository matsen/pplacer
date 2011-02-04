open OUnit;;

let suite = "all tests" >::: [
    "mokaphy" >::: Test_all_mokaphy.suite;
];;

let _ =
  run_test_tt_main suite
;;

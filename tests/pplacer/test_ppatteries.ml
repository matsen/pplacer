

open Ppatteries
open OUnit
let maybe_zipped_suite =
  let assert_false x = assert_equal x false
  and assert_true x = assert_equal x true in
  ["test_maybezipped_is_gzipped" >:: begin fun() ->
      assert_false (MaybeZipped.is_gzipped "test.ml");
      assert_true (MaybeZipped.is_gzipped "test.ml.gz");
    end;
   "test_maybezipped_drop_gz" >:: begin fun() ->
     assert_equal (MaybeZipped.drop_gz "test.ml") "test.ml";
     assert_equal (MaybeZipped.drop_gz "test.ml.gz") "test.ml";
   end;
   "test_maybezipped_check_suffix" >:: begin fun() ->
     assert_true (MaybeZipped.check_suffix "test.ml.gz" ".ml");
     assert_true (MaybeZipped.check_suffix "test.ml" ".ml");
   end;
   "test_maybezipped_chop_suffix" >:: begin fun() ->
     assert_equal (MaybeZipped.chop_suffix "test.ml.gz" ".ml") "test";
     assert_equal (MaybeZipped.chop_suffix "test.ml" ".ml") "test";
   end;
   "test_maybezipped_fasta" >:: begin fun() ->
     let fname = "tests/data/compress/hiv.fasta" in
     let a_text = Alignment.list_of_any_file fname
     and a_gz = Alignment.list_of_any_file (fname ^ ".gz") in
     List.iter2 assert_equal a_text a_gz;
   end;]

let suite = [
  "test_median" >:: begin fun () ->
    List.iter
      (Tuple.Tuple2.map1 median %> uncurry assert_equal)
      [
        [0], 0;
        [0; 1], 0;
        [0; 1; 2], 1;
        [0; 1; 2; 3], 1;
        [0; 1; 2; 3; 4], 2;
      ];
    assert_raises (Invalid_argument "median") (fun () -> median []);
  end;
] @ maybe_zipped_suite

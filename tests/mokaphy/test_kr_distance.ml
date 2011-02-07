open OUnit
open Test_util

let simple_expected = [
  (0.5, [
    ("test1", "test2", 0.686887);
    ("test1", "test3", 0.319036);
    ("test2", "test3", 0.367851);
   ]);
  (1.0, [
    ("test1", "test2", 0.583333);
    ("test1", "test3", 0.25);
    ("test2", "test3", 0.333333);
   ]);
  (2.0, [
    ("test1", "test2", 0.677003);
    ("test1", "test3", 0.408248);
    ("test2", "test3", 0.540062);
   ]);
]

let generate_tests which expected =
  let data = read_data which in
  which >::: List.map (fun (p, pairs) ->
    (Printf.sprintf "exp %f" p) >::: List.map (fun (pr_name1, pr_name2, expected) ->
      let (pr1, mass1), (pr2, mass2) = Hashtbl.find data pr_name1, Hashtbl.find data pr_name2 in
      let tree = Placerun.get_same_tree pr1 pr2 in
      let calculated = Kr_distance.dist_of_pres p tree mass1 mass2 in
      (Printf.sprintf "%s x %s" pr_name1 pr_name2) >:: fun _ ->
        (Printf.sprintf "%f !~= %f" calculated expected) @? approximately_equal expected calculated
    ) pairs;
  ) expected;
;;

let suite = [
  generate_tests "simple" simple_expected;
]

open OUnit
open Test_util

let simple_expected = [
  (0.5, [
    ("test1", "test2", 0.686887);
    ("test1", "test3", 0.319036);
    ("test2", "test3", 0.367851);
    ("test4", "test4_demulti", 0.);
  ]);
  (1.0, [
    ("test1", "test2", 0.583333);
    ("test1", "test3", 0.25);
    ("test2", "test3", 0.333333);
    ("test4", "test4_demulti", 0.);
  ]);
  (2.0, [
    ("test1", "test2", 0.677003);
    ("test1", "test3", 0.408248);
    ("test2", "test3", 0.540062);
    ("test4", "test4_demulti", 0.);
  ]);
]

let psbA_expected = [
  (0.5, [
    ("DCM", "coastal", 0.085899);
    ("DCM", "surface", 0.0679409);
    ("DCM", "upwelling", 0.0777173);
    ("coastal", "surface", 0.0910193);
    ("coastal", "upwelling", 0.0400257);
    ("surface", "upwelling", 0.0850928);
  ]);
  (1.0, [
    ("DCM", "coastal", 0.0341428);
    ("DCM", "surface", 0.014255);
    ("DCM", "upwelling", 0.0278099);
    ("coastal", "surface", 0.032223);
    ("coastal", "upwelling", 0.00951196);
    ("surface", "upwelling", 0.0270792);
  ]);
  (2.0, [
    ("DCM", "coastal", 0.120893);
    ("DCM", "surface", 0.0396716);
    ("DCM", "upwelling", 0.104636);
    ("coastal", "surface", 0.0971287);
    ("coastal", "upwelling", 0.0277832);
    ("surface", "upwelling", 0.0827712);
  ]);
]

let moran_expected = [
  (1.0, [
    ("control","de_rounded_control",5.29902e-07);
    ("control","de_rounded_dmsp",0.00660102);
    ("control","dmsp",0.00660101);
    ("control","rounded_control",5.29902e-07);
    ("control","rounded_dmsp",0.00660102);
    ("de_rounded_control","de_rounded_dmsp",0.00660069);
    ("de_rounded_control","dmsp",0.00660068);
    ("de_rounded_control","rounded_control",2.00699e-17);
    ("de_rounded_control","rounded_dmsp",0.00660069);
    ("de_rounded_dmsp","dmsp",3.09241e-08);
    ("de_rounded_dmsp","rounded_control",0.00660069);
    ("de_rounded_dmsp","rounded_dmsp",1.15665e-17);
    ("dmsp","rounded_control",0.00660068);
    ("dmsp","rounded_dmsp",3.09241e-08);
    ("rounded_control","rounded_dmsp",0.00660069);
  ]);
]

let generate_tests transform which expected =
  let data = pres_of_dir which in
  which >::: List.map (fun (p, pairs) ->
    (Printf.sprintf "exp %f" p) >::: List.map (fun (pr_name1, pr_name2, expected) ->
      let (pr1, mass1), (pr2, mass2) = Hashtbl.find data pr_name1, Hashtbl.find data pr_name2 in
      let tree = Placerun.get_same_tree pr1 pr2 in
      let calculated = 
        Kr_distance.dist_of_pres transform p tree mass1 mass2 in
      (Printf.sprintf "%s x %s" pr_name1 pr_name2) >:: fun _ ->
        (Printf.sprintf "%f !~= %f" calculated expected) @? approximately_equal expected calculated
    ) pairs;
  ) expected;
;;

let suite = [
  generate_tests Mass_map.no_transform "simple" simple_expected;
  generate_tests Mass_map.no_transform "psbA" psbA_expected;
  generate_tests Mass_map.no_transform "moran" moran_expected;
]

open OUnit
open Test_util


(* *** Predefined: tests where we have a fixed value we are aiming for *** *)

let criterion = Placement.ml_ratio

let predefined_tests transform_str which expected =
  let data = pres_of_dir Mass_map.Weighted criterion which
  and transform = Mass_map.transform_of_str transform_str
  in
  ("predefined_"^transform_str^"_"^which) >::: List.map (fun (p, pairs) ->
    (Printf.sprintf "exp %f" p) >::: List.map (fun (pr_name1, pr_name2, expected) ->
      let (pr1, pre1) = Hashtbl.find data pr_name1
      and (pr2, pre2) = Hashtbl.find data pr_name2
      in
      let tree = Placerun.get_same_tree pr1 pr2 in
      let normalization = Gtree.tree_length tree in
      let calculated =
        Kr_distance.dist_of_pres ~normalization transform p tree ~pre1 ~pre2 in
      (Printf.sprintf "%s x %s" pr_name1 pr_name2) >:: fun _ ->
        (Printf.sprintf "%f !~= %f" calculated expected) @? approx_equal expected calculated
    ) pairs;
  ) expected;
;;

(*
 * from R-verification.kr.R
 *
 *  [,1]      [,2]      [,3]
 *  0.5 0.6868867 0.5833333 0.6770032
 *  1   0.3190356 0.2500000 0.4082483
 *  2   0.3678511 0.3333333 0.5400617
 *)

let simple_no_trans_expected = [
  (0.5, [
    ("test1", "test2", 0.686887);
    ("test1", "test3", 0.319036);
    ("test2", "test3", 0.367851);
    ("test4", "test4_demulti", 0.);
  ]);
  (* note that total tree length is 24 *)
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


(*
 * from R-verification.kr.R:

0.3394316 0.2812500 0.4506939
*)

let simple_no_trans_expected = [
  (0.5, [
    ("test1", "test2", 0.686887);
    ("test1", "test3", 0.319036);
    ("test2", "test3", 0.367851);
    ("test4", "test4_demulti", 0.);
  ]);
  (* note that total tree length is 24 *)
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

let multi_notrans_expected =
  [
    0.5, [ "multi_test1", "test3", 0.3394316; ];
    1.0, [ "multi_test1", "test3", 0.2812500; ];
    2.0, [ "multi_test1", "test3", 0.4506939; ];
  ]

let no_transform_predefined_suite =
  List.map
    (fun (trans_str, n, pd) -> predefined_tests trans_str n pd)
    [
      "no_trans", "simple", simple_no_trans_expected;
      "no_trans", "psbA", psbA_expected;
      "no_trans", "moran", moran_expected;
      "no_trans", "simple", multi_notrans_expected;
    ]


(* *** Matrix tests: comparing to alternative formulation using matrices. *** *)

let hashtbl_keys h = Hashtbl.fold (fun k _ l -> k::l) h []

let get_pairs l =
  let rec aux = function
    | x::l -> (List.map (fun y -> (x,y)) l)::(aux l)
    | [] -> []
  in
  List.flatten (aux l)

let weighting = Mass_map.Unweighted

let matrix_tests which =
  let data = pres_of_dir weighting criterion which in
  let names = hashtbl_keys data in
  ("matrix_"^which) >:::
    List.map
      (fun (pr_name1, pr_name2) ->
        let (pr1, pre1) = Hashtbl.find data pr_name1
        and (pr2, pre2) = Hashtbl.find data pr_name2
        in
        let t = Placerun.get_same_tree pr1 pr2 in
        let normalization = Gtree.tree_length t in
        let kr =
          Kr_distance.dist_of_pres ~x1:1. ~x2:1.
            ~normalization Mass_map.no_transform 2. t ~pre1 ~pre2
        and matrix = Matrix_sig.matrix_distance weighting criterion pr1 pr2
        in
        (Printf.sprintf "%s x %s" pr_name1 pr_name2) >::
          fun _ ->
            (Printf.sprintf "%f !~= %f" kr matrix) @? approx_equal kr matrix)
      (get_pairs names)

let matrix_suite =
  List.map
    matrix_tests
    [
      "simple";
      "psbA"
    ]


(* *** the final suite *** *)
let suite =
  no_transform_predefined_suite
  @ matrix_suite


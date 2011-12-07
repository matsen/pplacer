(* Unit testing for guppy kr.
 *
 * The testing has two components. First, there are the "predefined" tests where
 * we have a fixed value we are aiming for. These are a combination of "simple"
 * which have been computed by hand (actually calculated in
 * R-verification.kr.R), and then "psbA" and "moran" which are pre-computed and
 * we want to make sure they stay the same. All of those are with weighted
 * placements, no multiplicity transformation, and use varying ps. Also within
 * the predefined tests are some by hand values for various transformations of
 * multiplicity.
 *
 * The second check are matrix tests which use an alternate method for computing
 * KR. This is just for the unweighted case with no multiplicity and p=2.
 *
 *)

open Ppatteries
open OUnit
open Test_util


(* *** Predefined: tests where we have a fixed value we are aiming for *** *)

let criterion = Placement.ml_ratio

let predefined_tests which expected =
  let data = pres_of_dir Mass_map.Spread criterion which
  in
  ("predefined_"^which) >::: List.map (fun (p, pairs) ->
    (Printf.sprintf "exp %f" p) >::: List.map (fun (pr_name1, pr_name2, expected) ->
      let (pr1, pre1) = Hashtbl.find data pr_name1
      and (pr2, pre2) = Hashtbl.find data pr_name2
      in
      let tree = Placerun.get_same_tree pr1 pr2 in
      let normalization = Gtree.tree_length tree in
      let calculated =
        Kr_distance.scaled_dist_of_pres ~normalization p tree pre1 pre2 in
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

let simple_expected = [
  (0.5, [
    ("test1", "test2", 0.686887);
    ("test1", "test3", 0.319036);
    ("test2", "test3", 0.367851);
  ]);
  (* note that total tree length is 24 *)
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

(* uptri versions of the previous data *)
let multi_dist_expected = [
  0.5, [|0.686887; 0.319036; 0.367851|];
  1.0, [|0.583333; 0.25; 0.333333|];
  2.0, [|0.677003; 0.408248; 0.540062|];
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

                 [,1]      [,2]      [,3]
no_trans    0.3394316 0.2812500 0.4506939
unit_trans  0.3190356 0.2500000 0.4082483
asinh_trans 0.3292013 0.2649481 0.4275588
*)

let multi_expected =
  [
    0.5, [ "multi_test1", "test3", 0.3394316; ];
    1.0, [
           "multi_test1", "test3", 0.2812500;
           "test4", "test4_demulti", 0.;
         ];
    2.0, [ "multi_test1", "test3", 0.4506939; ];
  ]

let predefined_suite =
  List.map
    (fun (n, pd) -> predefined_tests n pd)
    [
      "simple", simple_expected;
      "psbA", psbA_expected;
      "moran", moran_expected;
      "multi", multi_expected;
    ]


(* *** Matrix tests: comparing to alternative formulation using matrices. *** *)

let hashtbl_keys h = Hashtbl.fold (fun k _ l -> k::l) h []

let get_pairs l =
  let rec aux = function
    | x::l -> (List.map (fun y -> (x,y)) l)::(aux l)
    | [] -> []
  in
  List.flatten (aux l)

let point_spread = Mass_map.Point

let matrix_tests which =
  let data = pres_of_dir point_spread criterion which in
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
            ~normalization 2. t ~pre1 ~pre2
        and matrix = Matrix_sig.matrix_distance point_spread criterion pr1 pr2
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
  predefined_suite
  @ matrix_suite
  @ [
    "multi_dist_test" >:: begin fun () ->
      let prl = placeruns_of_dir "simple" in
      let gt = Mokaphy_common.list_get_same_tree prl in
      let prel = List.map
        (Mass_map.Pre.of_placerun Mass_map.Spread Placement.ml_ratio)
        prl
      and normalization = Gtree.tree_length gt in
      List.iter
        (fun (p, expected) ->
          (Printf.sprintf "exponent %g doesn't match" p)
          @? (Array.for_all2
                approx_equal
                (Kr_distance.scaled_multi_dist_of_pres ~normalization p gt prel
                 |> Uptri.to_array)
                expected))
        multi_dist_expected
    end;
  ]

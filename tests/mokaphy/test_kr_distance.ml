open OUnit
open Test_util

(* tests where we have calculated distance by hand *)

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

(* regression test *)

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

let named_predefined = [
  "simple", simple_expected;
  "psbA", psbA_expected;
  "moran", moran_expected;
]

let criterion = Placement.ml_ratio

let predefined_tests transform which expected =
  let data = pres_of_dir Mass_map.Weighted criterion which in
  ("predefined_"^which) >::: List.map (fun (p, pairs) ->
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

let hashtbl_keys h = Hashtbl.fold (fun k _ l -> k::l) h []

let get_pairs l =
  let rec aux = function
    | x::l -> (List.map (fun y -> (x,y)) l)::(aux l)
    | [] -> []
  in
  List.flatten (aux l)

(* totally inefficient! *)
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
          Kr_distance.scaled_dist_of_pres
            ~normalization Mass_map.no_transform 2. t pre1 pre2
        and matrix = Matrix_sig.matrix_distance weighting criterion pr1 pr2
        in
        (Printf.sprintf "%s x %s" pr_name1 pr_name2) >::
          fun _ ->
            (Printf.sprintf "%f !~= %f" kr matrix) @? approx_equal kr matrix)
      (get_pairs names)

let suite =
  (List.map
    (fun (n, pd) -> predefined_tests Mass_map.no_transform n pd)
    named_predefined) @
  (List.map
    (fun (n, _) -> matrix_tests n)
    [
      "simple", simple_expected;
      "psbA", psbA_expected;
    ])

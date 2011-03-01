open Mass_map

(* Assume the test runner is running in the project root. We can't do much
   better than this. *)
let tests_dir = "./tests/"

let pres_of_dir weighting criterion which =
  let files = Common_base.get_dir_contents
    ~pred:(fun name -> Filename.check_suffix name "place")
    (tests_dir ^ "mokaphy/data/" ^ which) in
  let tbl = Hashtbl.create 10 in
  List.iter
    (fun f ->
      let pr = Placerun_io.of_file f in
      let pre = Pre.normalize_mass no_transform (Pre.of_placerun weighting criterion pr) in
      Hashtbl.add tbl pr.Placerun.name (pr, pre))
    files;
  tbl
;;

let fabs x = if x > 0.0 then x else -. x;;

let approximately_equal ?(epsilon = 1e-5) f1 f2 = fabs (f1 -. f2) < epsilon;;

let gtree_equal g1 g2 =
  if g1.Gtree.stree = g2.Gtree.stree then
    MapsSets.IntMap.equal (fun b1 b2 -> (Newick_bark.compare b1 b2) = 0) g1.Gtree.bark_map g2.Gtree.bark_map
  else false

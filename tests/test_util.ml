(* Assume the test runner is running in the project root. We can't do much
   better than this. *)
let tests_dir = "./tests/"

let pres_of_dir which =
  let files = Common_base.get_dir_contents
    ~pred:(fun name -> Filename.check_suffix name "place")
    (tests_dir ^ "mokaphy/data/" ^ which) in
  let tbl = Hashtbl.create 10 in
  List.iter(fun f ->
    let pr = Placerun_io.of_file f in
    let pre = Mass_map.Pre.of_placerun Mass_map.Weighted Placement.ml_ratio pr in
    Hashtbl.add tbl pr.Placerun.name (pr, pre)
  ) files;
  tbl
;;

let approximately_equal ?(epsilon = 1e-5) f1 f2 = abs_float (f1 -. f2) < epsilon;;

let matrices_approximately_equal ?(epsilon = 1e-5) m1 m2 =
  let (rows,cols) as dim1 = Gsl_matrix.dims m1 in
  try
    assert(dim1 = Gsl_matrix.dims m2);
    for i=0 to rows-1 do
      for j=0 to cols-1 do
        if not (approximately_equal ~epsilon m1.{i,j} m2.{i,j}) then raise Exit
      done
    done;
    true
  with
  | Exit -> false

let ( ^=^ ) = matrices_approximately_equal

let gtree_equal g1 g2 =
  if g1.Gtree.stree = g2.Gtree.stree then
    MapsSets.IntMap.equal (fun b1 b2 -> (Newick_bark.compare b1 b2) = 0) g1.Gtree.bark_map g2.Gtree.bark_map
  else false

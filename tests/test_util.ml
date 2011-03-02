open Mass_map
open Fam_batteries

(* Assume the test runner is running in the project root. We can't do much
   better than this. *)
let tests_dir = "./tests/"

let placeruns_of_dir which =
  let files = Common_base.get_dir_contents
    ~pred:(fun name -> Filename.check_suffix name "json")
    (tests_dir ^ "data/" ^ which) in
  List.map
    Placerun_io.of_any_file
    files

let pres_of_dir weighting criterion which =
  let tbl = Hashtbl.create 10 in
  List.iter
    (fun pr ->
      let pre = Pre.normalize_mass no_transform (Pre.of_placerun weighting criterion pr) in
      Hashtbl.add tbl pr.Placerun.name (pr, pre))
    (placeruns_of_dir which);
  tbl

let approx_equal ?(epsilon = 1e-5) f1 f2 = abs_float (f1 -. f2) < epsilon;;

let vec_approx_equal ?(epsilon = 1e-5) v1 v2 =
  let dim = Gsl_vector.length v1 in
  try
    assert(dim = Gsl_vector.length v2);
    for i=0 to dim-1 do
      if not (approx_equal ~epsilon v1.{i} v2.{i}) then raise Exit
    done;
    true
  with
  | Exit -> false

let ( *=* ) = vec_approx_equal

let mat_approx_equal ?(epsilon = 1e-5) m1 m2 =
  let (rows,cols) as dim1 = Gsl_matrix.dims m1 in
  try
    assert(dim1 = Gsl_matrix.dims m2);
    for i=0 to rows-1 do
      for j=0 to cols-1 do
        if not (approx_equal ~epsilon m1.{i,j} m2.{i,j}) then raise Exit
      done
    done;
    true
  with
  | Exit -> false

let ( ^=^ ) = mat_approx_equal

let array_f_equal f a1 a2 =
  try
    ArrayFuns.iter2 (fun x y -> if not (f x y) then raise Exit) a1 a2;
    true
  with | Exit -> true

let farr_approx_equal ?(epsilon = 1e-5) fa1 fa2 =
  array_f_equal (approx_equal ~epsilon) fa1 fa2

let farrarr_approx_equal ?(epsilon = 1e-5) faa1 faa2 =
  array_f_equal (farr_approx_equal ~epsilon) faa1 faa2

let gtree_equal g1 g2 =
  g1.Gtree.stree = g2.Gtree.stree
  && MapsSets.IntMap.equal (fun b1 b2 -> (Newick_bark.compare b1 b2) = 0) g1.Gtree.bark_map g2.Gtree.bark_map

let placerun_equal pr1 pr2 =
  gtree_equal pr1.Placerun.ref_tree pr2.Placerun.ref_tree
  && pr1.Placerun.pqueries = pr2.Placerun.pqueries

(* For white space separated vectors and matrices.
 * These aren't very smart-- leading and trailing whitespace will annoy them.
 * *)

let farr_of_string s =
  Array.of_list (List.map float_of_string (Str.split (Str.regexp "[ ]+") s))

let farrarr_of_string s =
  Array.of_list (List.map farr_of_string (Str.split (Str.regexp "\n") s))

let vec_of_string s = Gsl_vector.of_array (farr_of_string s)
let mat_of_string s = Gsl_matrix.of_arrays (farrarr_of_string s)


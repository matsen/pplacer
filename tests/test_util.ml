open Mass_map
open Ppatteries
open OUnit

(* Assume the test runner is running in the project root. We can't do much
   better than this. *)
let tests_dir = "./tests/"


(* *** convenience funs for getting things *** *)

let placeruns_of_dir which =
  let files = Common_base.get_dir_contents
    ~pred:(fun name -> Filename.check_suffix name "jplace")
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

(* For white space separated vectors and matrices.
 * These aren't very smart-- leading and trailing whitespace will annoy them.
 * *)

let farr_of_string s =
  Array.of_list (List.map float_of_string (Str.split (Str.regexp "[ ]+") s))

let farrarr_of_string s =
  Array.of_list (List.map farr_of_string (Str.split (Str.regexp "\n") s))

let vec_of_string s = Gsl_vector.of_array (farr_of_string s)
let mat_of_string s = Gsl_matrix.of_arrays (farrarr_of_string s)


(* *** equalities *** *)
let gtree_equal g1 g2 =
  g1.Gtree.stree = g2.Gtree.stree
  && IntMap.equal (fun b1 b2 -> (Newick_bark.compare b1 b2) = 0) g1.Gtree.bark_map g2.Gtree.bark_map

let placerun_equal pr1 pr2 =
  gtree_equal pr1.Placerun.ref_tree pr2.Placerun.ref_tree
  && pr1.Placerun.pqueries = pr2.Placerun.pqueries


(* *** approximate equalities *** *)

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

let array_f_equal f a1 a2 =
  try
    ArrayFuns.iter2 (fun x y -> if not (f x y) then raise Exit) a1 a2;
    true
  with | Exit -> true

let farr_approx_equal ?(epsilon = 1e-5) fa1 fa2 =
  array_f_equal (approx_equal ~epsilon) fa1 fa2

let farrarr_approx_equal ?(epsilon = 1e-5) faa1 faa2 =
  array_f_equal (farr_approx_equal ~epsilon) faa1 faa2

exception Inequal of Jsontype.jsontype * Jsontype.jsontype
let rec json_equal ?(epsilon = 1e-5) j1 j2 =
  if begin match j1, j2 with
    | Jsontype.Bool b1, Jsontype.Bool b2 -> b1 = b2
    | Jsontype.Int i1, Jsontype.Int i2 -> i1 = i2
    | Jsontype.Float f1, Jsontype.Float f2 -> approx_equal ~epsilon f1 f2
    | Jsontype.String s1, Jsontype.String s2 -> s1 = s2
    | Jsontype.Object o1, Jsontype.Object o2 ->
      (Hashtbl.length o1) = (Hashtbl.length o2) && begin
        Hashtbl.iter
          (fun k v ->
            if not (Hashtbl.mem o2 k) then raise (Inequal (j1, j2));
            json_equal ~epsilon v (Hashtbl.find o2 k))
          o1;
        true
      end
    | Jsontype.Array a1, Jsontype.Array a2 ->
      (List.length a1) = (List.length a2) && begin
        List.iter2
          (json_equal ~epsilon)
          a1
          a2;
        true
      end
    | Jsontype.Null, Jsontype.Null -> true
    | _, _ -> false
  end then () else raise (Inequal (j1, j2))


(* *** infixes for equalities *** *)

let ( =| ) = vec_approx_equal
let ( =|| ) = mat_approx_equal
let ( =@ ) = farr_approx_equal
let ( =@@ ) = farrarr_approx_equal

let check_map_approx_equal message = Enum.iter2
  (fun (k1, v1) (k2, v2) ->
    (Printf.sprintf message k1 k2)
    @? (k1 = k2 && approx_equal v1 v2))

(* *** random stuff *** *)

let rand_symmetric n =
  let m = Gsl_matrix.create n n in
  for i=0 to n-1 do
    for j=i to n-1 do
      m.{i,j} <- 1. -. Random.float 2.;
      m.{j,i} <- m.{i,j};
    done;
  done;
  m;;

let make_rng seed =
  let rng = Gsl_rng.make Gsl_rng.KNUTHRAN2002 in
  Gsl_rng.set rng (Nativeint.of_int seed);
  rng


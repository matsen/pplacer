open Mass_map
open Ppatteries
open OUnit

(* Assume the test runner is running in the project root. We can't do much
   better than this. *)
let tests_dir = "./tests/"


(* *** convenience funs for getting things *** *)

let placeruns_of_dir which =
  get_dir_contents
    ~pred:(flip Filename.check_suffix "jplace")
    (tests_dir ^ "data/" ^ which)
  |> List.of_enum
  |> List.sort compare
  |> List.map Placerun_io.of_any_file

let placerun_of_dir dir which =
  placeruns_of_dir dir
    |> List.find (Placerun.get_name |- (=) which)

let pres_of_dir weighting criterion which =
  let tbl = Hashtbl.create 10 in
  List.iter
    (fun pr ->
      let pre = Pre.normalize_mass (Pre.of_placerun weighting criterion pr) in
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

let placement_equal p1 p2 =
  let open Placement in
  p1.location = p2.location
  && (p1.ml_ratio =~ p2.ml_ratio)
  && (p1.log_like =~ p2.log_like)
  && Option.eq ~eq:(=~) p1.post_prob p2.post_prob
  && Option.eq ~eq:(=~) p1.marginal_prob p2.marginal_prob
  && (p1.distal_bl =~ p2.distal_bl)
  && (p1.pendant_bl =~ p2.pendant_bl)
  && (p1.classif = p2.classif)
  && Option.eq ~eq:(fun (a1, b1) (a2, b2) -> a1 =~ a2 && b1 = b2) p1.map_identity p2.map_identity

let pquery_equal pq1 pq2 =
  let open Pquery in
  List.for_all2
    (fun (n1, m1) (n2, m2) -> n1 = n2 && m1 =~ m2)
    pq1.namlom
    pq2.namlom
  && List.for_all2 placement_equal pq1.place_list pq2.place_list

let placerun_equal pr1 pr2 =
  let open Placerun in
  gtree_equal pr1.ref_tree pr2.ref_tree
  && Option.eq ~eq:(IntMap.equal (=)) pr1.transm pr2.transm
  && List.for_all2 pquery_equal pr1.pqueries pr2.pqueries


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
    | Jsontype.String s1, Jsontype.String s2 -> s1 = s2
    | Jsontype.Int i1, Jsontype.Int i2 -> i1 = i2
    | Jsontype.Float f1, Jsontype.Float f2 -> approx_equal ~epsilon f1 f2
    | Jsontype.Int i, Jsontype.Float f
    | Jsontype.Float f, Jsontype.Int i ->
      approx_equal ~epsilon f (float_of_int i)
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
    (Printf.sprintf message k1 v1 k2 v2)
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

let colorset_of_strings = List.map Tax_id.of_string |- Convex.ColorSet.of_list

let simple_refpkg tree_string =
  Refpkg.of_path
    ~ref_tree:(Newick_gtree.of_string tree_string)
    (tests_dir ^ "data/simple.refpkg")

open Ppatteries
open OUnit
open Test_util

class heat_test rp =
object (self)
  inherit Guppy_heat.cmd ()

  method private get_rp = rp
  method named_trees_of_csv =
    self#csv_to_named_trees self#get_decor_ref_tree

end

let expected = [
  [];
  [Decor.red; Decor.Width 90.];
  [];
  [];
  [Decor.blue; Decor.Width 180.];
  [Decor.red; Decor.Width 30.];
]

let simple () =
  let rp = Refpkg.of_path (tests_dir ^ "data/heat/my.simple.refpkg") in
  let test_obj = new heat_test rp in
  let trees =
    test_obj#named_trees_of_csv
      (tests_dir ^ "data/heat/heat_test.csv")
  in
  let gt = snd (List.hd trees) in
  let decor =
    Enum.map
      (fun x -> x#get_decor)
      (IntMap.values (gt.Gtree.bark_map))
  in
  let _ = Enum.fold2
    (fun got expected n ->
      (Printf.sprintf "#%d" n) @? (got = expected);
      n + 1)
    0
    decor
    (List.enum expected)
  in ()

let suite = [
  "simple" >:: simple;
]

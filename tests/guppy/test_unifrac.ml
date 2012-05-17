open Ppatteries
open OUnit
open Test_util

let simple_expected = [
  "test1", "test2", 1.;
  "test1", "test3", 0.666667;
  "test2", "test3", 1.;
]

let suite = [
  "test_unifrac" >:: begin fun () ->
    let names, (prl, prel) =
      pres_of_dir Mass_map.Spread Placement.ml_ratio "simple"
        |> Hashtbl.enum
        |> Enum.uncombine
        |> (Array.of_enum *** (List.of_enum |- List.split))
    in
    let gt = Mokaphy_common.list_get_same_tree prl
      |> Newick_gtree.add_zero_root_bl
    and res = Hashtbl.create 8 in
    Guppy_unifrac.unifrac gt (List.map Mass_map.Indiv.of_pre prel)
      |> Uptri.iterij (fun i j x -> Hashtbl.add res (names.(i), names.(j)) x);
    flip List.iter simple_expected (fun (name1, name2, expected) ->
      let calculated = Hashtbl.find res (name1, name2) in
      (Printf.sprintf "%f !~= %f" calculated expected)
      @? approx_equal expected calculated)
  end;
]

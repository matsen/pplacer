open Ppatteries
open OUnit
open Test_util

open Convex

let suite = List.map
  (fun (before, reroot_at, after) ->
    let before' = Newick_gtree.of_string before
    and after' = Newick_gtree.of_string after in
    Printf.sprintf "%s@%d" before reroot_at >:: fun () ->
      before'
        |> Gtree.get_stree
        |> flip Stree.reroot reroot_at
        |> Gtree.set_stree before'
        |> (fun got ->
          "failed to verify stree rerooting"
          @? (Gtree.get_stree got = Gtree.get_stree after'));
      let got = Gtree.reroot before' reroot_at in
      "failed to verify gtree rerooting" @? (gtree_equal got after'))
  [
    "((A,B):2,(C,D):3,E):0", 2, "(((C{3},D{4}):3{5},E{6}):2{7},A{0},B{1}):0{2}";
    "((A,B),(C,D):2,E)", 5, "(((A{0},B{1}){2},E{6}):2{7},C{3},D{4}){5}";
    "((A,B),(C,D),E)", 7, "((A,B),(C,D),E)";
  ]

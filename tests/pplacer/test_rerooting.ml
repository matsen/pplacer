open Ppatteries
open OUnit
open Test_util

open Convex

let suite = List.map
  (fun (before, reroot_at, after) ->
    let before' = Newick_gtree.of_string before
    and after' = Newick_gtree.of_string after |> Gtree.get_stree in
    before'
      |> Gtree.get_stree
      |> flip Stree.reroot reroot_at
      |> fun got ->
        (Printf.sprintf "%s@%d" before reroot_at) >:: fun () ->
          "failed to verify rerooting" @? (got = after'))
  [
    "((A,B),(C,D),E)", 2, "(((C[3],D[4])[5],E[6])[7],A[0],B[1])[2]";
    "((A,B),(C,D),E)", 5, "(((A[0],B[1])[2],E[6])[7],C[3],D[4])[5]";
    "((A,B),(C,D),E)", 7, "((A,B),(C,D),E)";
  ]

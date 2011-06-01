open OUnit
open Test_util
open Placerun

(*
> 2 * ( 3 * .5 *.3 + 4 * .5 * .2 + 5 * .3 * .2)
[1] 2.3
*)

(* We just take the first pq of the placerun. *)
let test_edpl correct fname =
  let pr = Placerun_io.of_any_file fname in
  let dm = Edge_rdist.build_pairwise_dist pr.ref_tree in
  (Printf.sprintf "%f <> edpl %s" correct pr.name) @?
    (correct = Edpl.of_pquery Placement.ml_ratio dm (List.hd pr.pqueries))

let suite = [
  "edpl" >::: [
    "test4" >:: (fun _ -> test_edpl 2.3 (tests_dir ^ "data/misc/test4.json"));
  ]
]


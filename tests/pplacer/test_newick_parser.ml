open Ppatteries
open OUnit
open Test_util

let suite = List.map
  (fun (s, test_without_labels) ->
    s >:: begin fun () ->
      let gt = Newick_gtree.of_string s in
      let gt' = gt
        |> Newick_gtree.to_string ~with_edge_labels:true
        |> Newick_gtree.of_string
      in
      let gt'' = gt'
        |> Newick_gtree.to_string ~with_edge_labels:true
        |> Newick_gtree.of_string
      in
      "roundtrip failed" @? (gtree_equal gt gt');
      "second roundtrip failed" @?
        (gtree_equal gt gt'' && gtree_equal gt' gt'');
      if test_without_labels then begin
        let gt' = gt |> Newick_gtree.to_string |> Newick_gtree.of_string in
        let gt'' = gt' |> Newick_gtree.to_string |> Newick_gtree.of_string in
        "roundtrip failed" @? (gtree_equal gt gt');
        "second roundtrip failed" @?
          (gtree_equal gt gt'' && gtree_equal gt' gt'');
      end
    end)
  [
    "((A,A),(B,B));", true;
    "(:1,:2):3;", true;
    "((,),);", true;
    "((,)1,)2;", true;
    "(A:1[1],B:2[2]):3[3];", false;
    "((A[1],B[2])[3],(A[4],B[5])[6])[7];", false;
    "(A[2],B[1])[0];", false;
    "([2],[1])[0];", false;
  ]

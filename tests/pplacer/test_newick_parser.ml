open Ppatteries
open OUnit
open Test_util

let suite = List.map
  (fun (s, test_without_labels) ->
    s >:: begin fun () ->
      let gt = Newick_gtree.of_string s in
      let gt' = gt
        |> Newick_gtree.to_string ~with_node_numbers:true
        |> Newick_gtree.of_string
      in
      let gt'' = gt'
        |> Newick_gtree.to_string ~with_node_numbers:true
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
    "((,)1,)two;", true;
    "((,)[one],)[two];", true;
    "(A:1[1],B:2[2]):3[3];", true;
    "((A[1],B[2])[3],(A[4],B[5])[6])[7];", true;
    "(A[2],B[1])[0];", true;
    "([2],[1])[0];", true;
    "(A:1{1},B:2{2}):3{3};", false;
    "((A{1},B{2}){3},(A{4},B{5}){6}){7};", false;
    "(A{2},B{1}){0};", false;
    "({2},{1}){0};", false;
    "(A:1{2}[3],)x:4{5}[6];", false;
  ]

let suite = suite @ [
  "test_legacy_format" >:: begin fun () ->
    let gt = Newick_gtree.of_string ~legacy_format:true "(A,B)[10];" in
    "top_id didn't match" @? (Gtree.top_id gt = 10)

  end;
]

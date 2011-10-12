open OUnit
open Test_util

let unlabeled = "((A:2,B:9):7,C:5,D:1):0;"
let labeled = "((A:2{0},B:9{1}):7{2},C:5{3},D:1{4}):0{5};"
let higher_labeled = "((A:2{20},B:9{21}):7{22},C:5{23},D:1{24}):0{25};"

let test_parsed_equality _ =
  let p1, p2 =
    Newick_gtree.of_string unlabeled, Newick_gtree.of_string labeled in
  "labeled != unlabled" @? gtree_equal p1 p2

let test_string_representation _ =
  let p1 = Newick_gtree.of_string higher_labeled in
  let s1 = Newick_gtree.to_string ~with_node_numbers:true p1 in
  let p2 = Newick_gtree.of_string s1 in
  let s2 = Newick_gtree.to_string ~with_node_numbers:true p2 in
  "first-parsed != second-parsed" @? gtree_equal p1 p2;
  "first-string != second-string" @? (s1 = s2)

let suite = [
  "parsed_equality" >:: test_parsed_equality;
  "string_representation" >:: test_string_representation;
]

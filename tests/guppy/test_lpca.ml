open Test_util
open Lpca
open OUnit

let x = mat_of_string
"9 3 2 7
0 4 8 2
0 0 5 3
0 0 0 1"

let r_x = mat_of_string
"9 3 2 7
3 4 8 2
2 8 5 3
7 2 3 1"

let test_rep_uptri _ =
  mat_rep_uptri x;
  "r_x <> mat_rep_uptri x" @? mat_approx_equal r_x x

let suite = [
  "rep_uptri" >:: (fun _ -> test_rep_uptri ());
]

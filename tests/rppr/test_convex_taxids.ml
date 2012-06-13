open OUnit
open Test_util
open Ppatteries

let printer sll =
  let ch = IO.output_string () in
  Printf.fprintf ch "%a" (List.print (List.print String.print)) sll;
  IO.close_out ch

let make_test tree expected () =
  simple_refpkg tree
    |> Rppr_convex_taxids.of_refpkg
    |> List.of_enum
    |> List.sort (List.make_compare String.compare)
    |> assert_equal ~printer expected


let suite = [
  "convex_tree" >:: make_test
    "((C_1,D_1),(F_1,G_1));"
    [
      ["family"; "A"];
      ["genus"; "B"];
      ["genus"; "E"];
      ["species"; "C"];
      ["species"; "D"];
      ["species"; "F"];
      ["species"; "G"];
    ];

  "nonconvex_species_tree" >:: make_test
    "((C_1,D_1),(C_2,D_2));"
    [
      ["family"; "A"];
      ["genus"; "B"];
    ];

  "one_convex_species_tree" >:: make_test
    "((C_1,(F_1,D_1)),(C_2,D_2));"
    [
      ["family"; "A"];
      ["genus"; "B"];
      ["genus"; "E"];
      ["species"; "F"];
    ];

  "one_convex_species_with_more_discordance_tree" >:: make_test
    "((C_1,(F_1,G_1)),(C_2,G_2));"
    [
      ["family"; "A"];
      ["species"; "F"];
    ];

]

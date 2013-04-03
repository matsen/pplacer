open OUnit
open Test_util
open Ppatteries

let printer sll =
  let ch = IO.output_string () in
  Printf.fprintf ch "%a"
    (List.print (Tuple3.print String.print String.print Int.print))
    sll;
  IO.close_out ch

let make_test tree expected () =
  simple_refpkg tree
    |> Rppr_convex_taxids.of_refpkg
    |> Enum.map (Tuple3.map2 Tax_id.to_string)
    |> List.of_enum
    |> List.sort (comparing Tuple3.first)
    |> assert_equal ~printer expected


let suite = [
  "convex_tree" >:: make_test
    "((C_1,D_1),(F_1,G_1));"
    [
      "family", "A", 4;
      "genus", "B", 2;
      "genus", "E", 2;
      "species", "C", 1;
      "species", "D", 1;
      "species", "F", 1;
      "species", "G", 1;
    ];

  "nonconvex_species_tree" >:: make_test
    "((C_1,D_1),(C_2,D_2));"
    [
      "family", "A", 4;
      "genus", "B", 4;
    ];

  "one_convex_species_tree" >:: make_test
    "((C_1,(F_1,D_1)),(C_2,D_2));"
    [
      "family", "A", 5;
      "genus", "B", 4;
      "genus", "E", 1;
      "species", "F", 1;
    ];

  "one_convex_species_with_more_discordance_tree" >:: make_test
    "((C_1,(F_1,G_1)),(C_2,G_2));"
    [
      "family", "A", 5;
      "species", "F", 1;
    ];

]

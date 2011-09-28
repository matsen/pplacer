open OUnit
open Test_util
open Ppatteries

let suite = [
  "edge_painting" >:: begin fun () ->
    let rp = simple_refpkg "((C_1,D_1),(F_1,F_2));" in
    let painted = Edge_painting.of_refpkg rp in
    "painted doesn't match"
    @? (List.enum
          [
            0, "B";
            1, "B";
            2, "A";
            3, "F";
            4, "F";
            5, "A";
          ]
        |> Enum.map (second Tax_id.of_string)
        |> IntMap.of_enum
        |> IntMap.equal (=) painted)
  end;

  "unrooted_edge_painting" >:: begin fun () ->
    let rp = simple_refpkg "(C_1,(D_1,(F_1,F_2)));" in
    let painted = Edge_painting.of_refpkg rp in
    "painted doesn't match"
    @? (List.enum
          [
            0, "B";
            1, "B";
            2, "F";
            3, "F";
            4, "A";
            5, "B";
          ]
        |> Enum.map (second Tax_id.of_string)
        |> IntMap.of_enum
        |> IntMap.equal (=) painted)
  end;

]

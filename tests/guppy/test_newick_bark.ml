open Ppatteries
open OUnit

let suite = [
  "test_get_confidence_name_opt" >:: begin fun () ->
    let cmp = Tuple2.compare
      ~cmp1:(Option.compare ~cmp:approx_compare)
      ~cmp2:(Option.compare ~cmp:String.compare)
    |-- (=) 0
    and printer x = IO.output_string ()
    |> tap (flip (Tuple2.print (Option.print Float.print) (Option.print String.print)) x)
    |> IO.close_out
    and make_bark ?n ?e () =
      new Newick_bark.newick_bark (`Of_bl_node_edge_label (None, n, e))
    in
    let (^=) = assert_equal ~cmp ~printer in

    let bark = make_bark () in
    bark#get_confidence_name_opt false ^= (None, None);
    bark#get_confidence_name_opt true ^= (None, None);
    let bark = make_bark ~n:"A" () in
    bark#get_confidence_name_opt false ^= (None, Some "A");
    bark#get_confidence_name_opt true ^= (None, Some "A");
    let bark = make_bark ~e:"A" () in
    bark#get_confidence_name_opt false ^= (None, None);
    bark#get_confidence_name_opt true ^= (None, None);
    let bark = make_bark ~n:"1" () in
    bark#get_confidence_name_opt false ^= (Some 1., None);
    bark#get_confidence_name_opt true ^= (None, Some "1");
    let bark = make_bark ~e:"1" () in
    bark#get_confidence_name_opt false ^= (Some 1., None);
    bark#get_confidence_name_opt true ^= (Some 1., None);
    let bark = make_bark ~n:"A" ~e:"B" () in
    bark#get_confidence_name_opt false ^= (None, Some "A");
    bark#get_confidence_name_opt true ^= (None, Some "A");
    let bark = make_bark ~n:"A" ~e:"1" () in
    bark#get_confidence_name_opt false ^= (Some 1., Some "A");
    bark#get_confidence_name_opt true ^= (Some 1., Some "A");
    let bark = make_bark ~n:"1" ~e:"A" () in
    bark#get_confidence_name_opt false ^= (Some 1., None);
    bark#get_confidence_name_opt true ^= (None, Some "1");
    let bark = make_bark ~n:"2" ~e:"1" () in
    bark#get_confidence_name_opt false ^= (Some 1., Some "2");
    bark#get_confidence_name_opt true ^= (Some 1., Some "2");

  end;

]

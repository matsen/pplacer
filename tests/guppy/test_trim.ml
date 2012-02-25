open Ppatteries
open OUnit
open Test_util

let suite = [
  "test_compress" >:: begin fun () ->
    let pr = placerun_of_dir "trim" "trim_in" in
    let pr', discarded = Guppy_trim.trim
      0.1
      true
      (Mass_map.Spread)
      (Placement.ml_ratio)
      (Placerun.get_ref_tree pr)
      (Placerun.get_pqueries pr)
    and expected = placerun_of_dir "trim" "trim_out" in
    "not equal" @? (placerun_equal pr' expected);
    "not empty" @? (RefList.is_empty discarded);

  end;

]

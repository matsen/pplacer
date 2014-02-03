open OUnit
open Test_util

let test_mask_reindex _ =
  "mask_index_empty" @?
    ([||] = Core.mask_reindex [|false; false;|]);
  "mask_index_6" @?
    ([|0; 3; 5|] = Core.mask_reindex [|true; false; false; true; false; true;|])

let suite = [
  "mask_index_reindex" >:: test_mask_reindex;
]


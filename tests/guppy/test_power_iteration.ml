open OUnit
open Test_util
open Pca
open Linear_utils
open Power_iteration

let () = Random.init 1;;
let m = rand_symmetric 5;;

let power_l, power_v = power_eigen 5 m;;
let symmv_l, symmv_v = symmv_eigen 5 m;;

let first_coord_pos_arr a =
  if a.(0) < 0. then Array.map (fun x -> -. x) a
  else a

let first_coord_pos_arr_arr aa =
  Array.map first_coord_pos_arr aa

let test_eigen l1 l2 v1 v2 =
  "eigenvalues not equal" @? (l1 =@ l2);
  "eigenvectors not equal" @?
    (first_coord_pos_arr_arr v1 =@@ first_coord_pos_arr_arr v2);
  ()

let suite = [
  "random 5x5" >:: (fun _ -> test_eigen symmv_l power_l symmv_v power_v)
]

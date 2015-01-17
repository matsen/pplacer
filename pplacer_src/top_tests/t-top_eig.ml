open Top_eig;;
open Gsl.Blas;;

let v = Gsl.Vector.of_array [|1.;2.;3.|];;
let n = Gsl.Blas.nrm2 v;;
scale_by_l2 v;;
v;;

(* top eigenvalue 8, with eigenvector (2,1,2) *)
let m =
  Gsl.Matrix.of_arrays
    [|
      [| 3.; 2.; 4.; |];
      [| 2.; 0.; 2.; |];
      [| 4.; 2.; 3.; |];
    |];;

let w = Fam_vector.mimic v;;
gemv NoTrans ~alpha:1. ~a:m ~x:v ~beta:0. ~y:w;;
w;;

top_eig m 1e-9 300;;

let random_symmetric n =
  let m = Gsl.Matrix.create n n in
  for i=0 to n-1 do
    for j=i to n-1 do
      let x = Random.float 1. in
      Bigarray.Array2.unsafe_set m i j x;
      Bigarray.Array2.unsafe_set m j i x;
    done;
  done;
  m

let compare size =
  let m = random_symmetric size in
  let time = Sys.time () in
  let ours = top_eig m 1e-3 500 in
  Printf.printf "ours took %g\n" ((Sys.time ()) -. time);
  let time = Sys.time () in
  let theirs = Gsl.Eigen.symm (`M(m)) in
  Printf.printf "theirs took %g\n" ((Sys.time ()) -. time);
  (ours, Gsl.Vector.max theirs);;

let test size =
  let m = random_symmetric size in
  top_eig m 1e-10 100;;

test 30;;
compare 1500;;


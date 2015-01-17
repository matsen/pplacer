open Test_util
open Pca
open OUnit


(*
a <- c(9,3,5,0)
b <- c(3,4,1,4)
c <- c(2,9,4,8)
m <- cbind(a, b, c)
m
#      a b c
# [1,] 9 3 2
# [2,] 3 4 9
# [3,] 5 1 4
# [4,] 0 4 8
*)

let x = farrarr_of_string
"9 3 2
3 4 9
5 1 4
0 4 8"

(*
cov(m)
#            a         b         c
# a  14.250000 -2.333333 -10.91667
# b  -2.333333  2.000000   3.00000
# c -10.916667  3.000000  10.91667
*)
let cov = covariance_matrix x

let r_cov = mat_of_string
"14.250000 -2.333333 -10.91667
-2.333333  2.000000   3.00000
-10.916667  3.000000  10.91667"

let test_covariance _ =
  "r_cov <> cov" @? mat_approx_equal r_cov cov


(*
show(prcomp(m))
# Standard deviations:
# [1] 4.9245017 1.4857642 0.8416972
#
# Rotation:
#          PC1        PC2        PC3
# a -0.7445646 -0.5718874 -0.3443375
# b  0.1652807 -0.6576928  0.7349302
# c  0.6467657 -0.4902907 -0.5842167
*)
let prcomp_stddevs = farr_of_string
"4.9245017 1.4857642 0.8416972"

(* note flipped sign of PC2 *)
let prcomp_vects = mat_of_string
"-0.7445646 0.5718874 0.3443375
  0.1652807 0.6576928 -0.7349302
  0.6467657 0.4902907 0.5842167"
(* we have the vectors be rows in matrix, so we can get at them *)
let () = Gsl.Matrix.transpose_in_place prcomp_vects

let (variances, pv) = gen_pca ~use_raw_eval:true 3 x;;
let vects = Gsl.Matrix.of_arrays pv;;
let stddevs = Array.map sqrt variances;;

(*
show(prcomp(m, scale=TRUE))
# Standard deviations:
# [1] 1.5230503 0.7690720 0.2980708
#
# Rotation:
#          PC1        PC2        PC3
# a -0.5872014 -0.5314280 -0.6105561
# b  0.5030103 -0.8305398  0.2391325
# c  0.6341729  0.1666971 -0.7550079
*)
let sprcomp_stddevs = farr_of_string
"1.5230503 0.7690720 0.2980708"

(* note sign of PC3 flipped *)
let sprcomp_vects = mat_of_string
"-0.5872014 0.5314280  0.6105561
0.5030103 0.8305398   -0.2391325
0.6341729 -0.1666971   0.7550079"
(* we have the vectors be rows in matrix, so we can get at them *)
let () = Gsl.Matrix.transpose_in_place sprcomp_vects

let (svariances, spv) = gen_pca ~scale:true ~use_raw_eval:true 3 x;;
let svects = Gsl.Matrix.of_arrays spv;;
let sstddevs = Array.map sqrt svariances;;

let test_pca stddev1 stddev2 vect1 vect2 =
  "stddevs not equal" @? farr_approx_equal stddev1 stddev2;
  "vects not equal" @? mat_approx_equal vect1 vect2;
  ()

let suite = [
  "unscaled" >:: (fun _ -> test_pca prcomp_stddevs stddevs prcomp_vects vects);
  "scaled" >:: (fun _ -> test_pca sprcomp_stddevs sstddevs sprcomp_vects svects);
]


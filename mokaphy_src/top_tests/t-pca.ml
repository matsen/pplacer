open Pca

let build_faa intll = 
  Array.of_list 
    (List.map 
      (fun l -> Array.of_list (List.map float_of_int l))
      intll)

(*
a <- c(9,3,5,0)
b <- c(3,4,1,4)
c <- c(2,9,4,8)
m <- cbind(a, b, c)
cov(m)
#     a b
# [1,] 9 3
# [2,] 3 4
# [3,] 5 1

cov(m)
#           a          b
# a  9.3333333 -0.6666667
# b -0.6666667  2.3333333



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

cov(m)
#            a         b         c
# a  14.250000 -2.333333 -10.91667
# b  -2.333333  2.000000   3.00000
# c -10.916667  3.000000  10.91667

show(prcomp(m))
# Standard deviations:
# [1] 4.9245017 1.4857642 0.8416972
# 
# Rotation:
#          PC1        PC2        PC3
# a -0.7445646 -0.5718874 -0.3443375
# b  0.1652807 -0.6576928  0.7349302
# c  0.6467657 -0.4902907 -0.5842167

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

let x = 
  build_faa 
  [
    [9; 3; 2];
    [3; 4; 9];
    [5; 1; 4];
    [0; 4; 8];
  ]

let cov = covariance_matrix x

let p = gen_pca x
let stddevs = Array.map sqrt (fst p)

let p = gen_pca ~scale:true x
let stddevs = Array.map sqrt (fst p)


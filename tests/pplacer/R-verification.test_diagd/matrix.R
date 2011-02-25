
library(Matrix)

b = as.matrix(read.table("b_matrix.tab"))
d = as.matrix(read.table("d_matrix.tab"))

a = d %*% b

expm(a)
expm(4*a)
eigen(b,symmetric=TRUE)

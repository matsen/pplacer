
# outside does the final finishing
outer_exp = function (p) { min(1,1/p) }
outside = function (p, inside) { (inside / tree_len) ** (outer_exp(p)) }

exponents = c(0.5, 1, 2)
tree_len = 24


# for the "usual" setup of no multiplicity
f = function (p) {
  m = 0.5^p
  c(
    outside(p, m * (1+7) + 7 + 1 + m * (0+4)),
    outside(p, m * (1+7) + 2),
    outside(p, m * (0+4) + 1 + 5)
  )
}
answers = rbind(sapply(exponents,f))
rownames(answers) = exponents
answers


# now say we have multiplicity 3 on one_x and multiplicity 5 on one_y
f = function (p) {
  mx = (3/8)^p
  my = (5/8)^p
  outside(p, mx*1 + my*7 + 2)
}
sapply(exponents,f)


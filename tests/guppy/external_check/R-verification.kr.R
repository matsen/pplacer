
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

no_trans = function (x) = x
f = function (p) {
  g = function (mx,my) {
    outside(p, (mx^p)*1 + (my^p)*7 + 2)
  }
  asx = asinh(3)
  asy = asinh(5)
  as_tot = asx + asy
  c(
   # here we have the multiplicity interpreted literally
    g(3/8,5/8),
   # hear each pquery gets one vote
    g(1/2,1/2),
   # and here is the asinh
    g(asx/as_tot, asy/as_tot)
  )
}
answers = rbind(sapply(exponents,f))
rownames(answers) = c("no_trans", "unit_trans", "asinh_trans")
answers


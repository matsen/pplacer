
outer_exp = function (p) { min(1,1/p) }

tree_len = 24

# outside does the final finishing
outside = function (p, inside) { (inside / tree_len) ** (outer_exp(p)) }

f = function (p) {
  m = 0.5^p
  c(
    outside(p, m * (1+7) + 7 + 1 + m * (0+4)),
    outside(p, m * (1+7) + 2),
    outside(p, m * (0+4) + 1 + 5)
  )
}

exponents = c(0.5, 1, 2)
answers = rbind(sapply(exponents,f))
rownames(answers) = exponents
answers


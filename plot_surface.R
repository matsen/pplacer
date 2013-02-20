#!/usr/bin/env Rscript

library(lattice)
library(plyr)

load_likelihoods <- function(n00, n01, n10, n11, r, b, t, rx, bx) {
  cmd <- paste('./lcfit_cli.native', n00, n01, n10, n11, r, b, t, rx, bx)
  res <- system(cmd, intern=TRUE)
  df <- read.csv(textConnection(res), header=FALSE)
  colnames(df) <- c('dist_bl', 'pend_bl', 'll')
  transform(df, cmd=cmd)
}

l <- list(0.01, 0.1, 1.0)

pdf('lcfittin.pdf')

l_ply(l, function(i) {
  likes <- load_likelihoods(1500, 300, 300, 300, 1, 0.5, 0.3, i, 0.5)
  print(wireframe(ll~dist_bl*pend_bl,
                  likes,
                  screen=list(z=-120, x=-70, y=0),
                  main=sprintf("cmd=%s", likes$cmd[1]),
                  drape=TRUE,
                  scales=list(arrows=FALSE)))
})

l <- list(0.01, 0.1, 0.5, 1.0)
l_ply(l, function(i) {
  likes <- load_likelihoods(1500, 300, 300, 300, 1, 0.5, 0.3, 0.3, i)
  print(wireframe(ll~dist_bl*pend_bl,
                  likes,
                  screen=list(z=-120, x=-70, y=0),
                  main=sprintf("cmd=%s", likes$cmd[1]),
                  drape=TRUE,
                  scales=list(arrows=FALSE)))
})

dev.off()

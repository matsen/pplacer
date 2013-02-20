#!/usr/bin/env Rscript

library(ggplot2)
library(gridExtra)

d <- read.csv('marg_like_comp.csv', as.is=TRUE, header=FALSE,
              col.names=c('name', 'quadrature_marginal_like', 'quadrature_pp',
                          'lcfit_marginal_like', 'lcfit_pp'))

theme_set(theme_bw(19))

p1 <- ggplot(d, aes(x=quadrature_marginal_like, y=lcfit_marginal_like)) +
  geom_abline(slope=1, color='grey') +
  geom_point(alpha=0.6) +
  xlab('quadrature') +
  ylab('lcfit') +
  ggtitle('Marginal Likelihood')

p2 <- ggplot(d, aes(x=quadrature_pp, y=lcfit_pp)) +
  geom_abline(slope=1, color='grey') +
  geom_point(alpha=0.6) +
  xlab('quadrature') +
  ylab('lcfit') +
  ggtitle('Posterior Probability') +
  xlim(0, 1) +
  ylim(0, 1)

svg('marg_like_comp.svg', width=14, height=7)
grid.arrange(p1, p2, nrow=1)
dev.off()

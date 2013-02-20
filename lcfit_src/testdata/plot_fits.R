#!/usr/bin/env Rscript

library(lattice)
library(ggplot2)
library(reshape2)
library(plyr)

tdat <- read.csv('marg_like_fit.csv', as.is=TRUE)

pdf('marg_like_fit.pdf', width=7, height=7)

melted <- melt(tdat, measure.vars=c('ll', 'fit_ll'))

d_ply(melted, .(pquery, pos), function(piece) {
  message(paste(piece$pquery[1], piece$pos[1]))
  if(piece$type[1] == 'pair' || length(unique(piece$dist_bl)) == 1) {
    p <- ggplot(piece, aes(x=pend_bl, y=value, color=variable, shape=variable)) +
      geom_point(alpha=0.7) +
      xlab("Pendant BL") +
      ggtitle(sprintf("Location %s.%d: LL", piece$pquery[1], piece$pos[1]))
    print(p)
  } else {
    scr <- list(z=-120, x=-70, y=0)

    w1 <- cloud(value~dist_bl*pend_bl,
                piece,
                group=variable,
                pch=c(15,16),
                screen=scr,
                main=sprintf("Location %s.%d: LL", piece$pquery[1], piece$pos[1]),
                scales=list(arrows=FALSE),
                auto.key=TRUE)

    #print(w1, position=c(0.0, 0, 1/2, 1), more=TRUE)
    print(w1)

    #w2 <- cloud(value~dist_bl*pend_bl,
                #piece,
                #drape=TRUE,
                #screen=scr,
                #main=sprintf("Location %s.%d: Fit LL", piece$pquery[1], piece$pos[1]),
                #scales=list(arrows=FALSE))
    #print(w2, position=c(1/2, 0, 1, 1))
  }
})


dev.off()

#!/usr/bin/env python
library(ggplot2)

theme_set(theme_bw(19))

timing <- read.csv('marg_like_timing.csv', as.is=TRUE)
timing <- subset(timing, !(subtype == 'tripod' & step == 'find_points_fit_model'))
timing <- transform(timing, label=sprintf('%s-%d', pquery, pos))

# Order by quadrature time
# GLKT0ZE01CEYOL,828,quadrature,,calc_marg_prob,0.602741003036
quad_time <- subset(timing, component=='quadrature', select=c(label, time))
name_order <- quad_time$label[order(quad_time$time)]

timing <- transform(timing, label=factor(label, levels=name_order))

p <- ggplot(timing, aes(x=label, y=time, fill=step)) +
  geom_bar(stat='identity') +
  facet_wrap(~component, ncol=1) +
  theme(axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank()) +
  xlab("") +
  ggtitle("Timing by placement")



svg('marg_like_timing.svg', width=20)
print(p)
dev.off()

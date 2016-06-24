my.data <- read.csv("../Analysis/huggins-results.csv")

library(ggplot2)

### Effective sample size = 1,728 (capture histories * nights trapped)

dodge <- position_dodge(width=0.9)
ggplot(my.data, aes(Time, Nhat, fill=Treatment)) +
  geom_bar(position="dodge", stat="identity") + facet_wrap(~Plot) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), position=dodge, width=0.25)


ggplot(subset(sum.data, species=="DIIN"), aes(factor(time), countsp, fill=treatment)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~plot) +
  labs(x="", y="Minimum number alive") + scale_fill_grey(start=0.5, end=0) + theme_bw() +
  theme(legend.title=element_blank())

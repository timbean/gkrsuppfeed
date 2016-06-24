### GKR Preliminary Data Analysis

library(ggplot2)
library(plyr)
library(tidyr)
library(reshape)
library(lmer)

#######
# 1. Load and clean data
#######

### Load all Lokern data
my.data <- read.csv("../Data/supp-data-combined.csv")
my.data$weight <- my.data$total.weight - my.data$bag.weight
my.data$plot2 <- mapvalues(my.data$plot, from=c("1", "2", "4", "IV", "P88", "P89", "S1", "S2", "S3"), 
                               to=c("Lokern-1", "Lokern-2", "Lokern-4", "Panoche-IV",
                                    "Panoche-88", "Panoche-89", "Carrizo-S1", "Carrizo-S2",
                                    "Carrizo-S3"))


#######
# 2. Visualize prelim results (total captured at each site, movement, body weight,
#        repro success)
#######

# Subtract minimun number of alive across years/plots...
mna <- subset(my.data, type=="N" | type=="S")
mna$species <- factor(mna$species)
#table(mna$plot, mna$time, mna$treatment, mna$species)
#test <- ddply(mna, .(time, site), summarize, count(species))
#test <- aggregate(mna$species ~ mna$site + mna$time, FUN="count")
#test <- cast(mna, site ~ time, value=species, count)

# sum.data <- NULL
# for(i in levels(mna$species)){
#   cur.spec <- subset(mna, species==i)
#   for(j in unique(cur.spec$plot)){
#     cur.plot <- subset(cur.spec, plot==j)
#     for(k in unique(cur.plot$treatment)){
#       cur.treatment <- subset(cur.plot, treatment==k)
#       for(l in unique(cur.plot$time)){
#         cur.time <- subset(cur.treatment, time==l)
#         sum.data <- rbind(sum.data, data.frame(i,j,k,l, nrow(cur.time)))
#       }
#     }
#   }
# }
# colnames(sum.data) <- c("species", "plot", "treatment", "time", "count")

plot <- rep(rep(levels(mna$plot), each=4), nlevels(mna$species))
species <- rep(levels(mna$species), each=(4*nlevels(mna$plot)))
treatment <- rep(c("treatment", "control"), length(species)/2)
time <- rep(c("before", "before", "after", "after"), length(species)/4)
countsp <- rep(NA, length(time))
sum.data <- data.frame(plot, species, treatment, time, countsp)
for(i in 1:nrow(sum.data)){
  cur.row <- sum.data[i,]
  cur.data <- subset(mna, species==cur.row$species)
  cur.data <- subset(cur.data, time==cur.row$time)
  cur.data <- subset(cur.data, plot==cur.row$plot)
  cur.data <- subset(cur.data, treatment==cur.row$treatment)
  sum.data$countsp[i] <- nrow(cur.data)
  #sum.data$countsp[i] <- length(unique(cur.data$left.ear))
}

plot <- rep(levels(mna$plot), nlevels(mna$species))
species <- rep(levels(mna$species), each=nlevels(mna$plot))
diffs <- rep(NA, length(species))
diff.data <- data.frame(plot, species, diffs)
for(i in 1:nrow(diff.data)){
  cur.row <- diff.data[i,]
  cur.plot <- subset(sum.data, plot==cur.row$plot)
  cur.plot <- subset(cur.plot, species==cur.row$species)
  diff.data$diffs[i] <- (cur.plot$countsp[3] - cur.plot$countsp[4]) - 
    (cur.plot$countsp[1] - cur.plot$countsp[2])
}

ggplot(diff.data, aes(factor(plot), diffs)) + geom_bar(stat="identity") + 
  facet_wrap(~species)

ggplot(subset(diff.data, species=="DIIN"), aes(factor(plot), diffs)) + geom_bar(stat="identity") + 
  facet_wrap(~species)

ggplot(subset(diff.data, species!="DIIN"), aes(factor(plot), diffs)) + geom_bar(stat="identity") + 
  facet_wrap(~species)

# Plot differences in MNA GKR before/after vs. control/treatment
sum.data$time <- with(sum.data, factor(time, levels=rev(levels(time))))
ggplot(subset(sum.data, species=="DIIN"), aes(factor(time), countsp, fill=treatment)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~plot) +
  labs(x="", y="Minimum number alive") + scale_fill_grey(start=0.5, end=0) + theme_bw() +
  theme(legend.title=element_blank())

ggplot(subset(sum.data, species=="DIIN"), aes(factor(treatment), countsp, fill=time)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~plot) +
  labs(x="", y="Minimum number alive")

ggplot(diff.data, aes(factor(species), diffs)) + geom_point(stat="identity")

boxplot(diff.data$diffs ~ diff.data$species)

ggplot(mna, aes(factor(time), weight)) + geom_point() +
  facet_wrap(~treatment + species)

### Compare weights betwen treatment/control for GKR
gkr.data <- subset(my.data, species=="DIIN")
gkr.data <- subset(gkr.data, age=="A")
gkr.data <- subset(gkr.data, repro != "P")
gkr.data <- subset(gkr.data, sex == "M" | sex == "F")
gkr.data <- gkr.data[gkr.data$weight > 100,]

weight.diffs <- NULL
for(i in unique(gkr.data$plot)){
  cur.plot <- subset(gkr.data, plot==i)
  for(j in unique(cur.plot$left.ear)){
    cur.rat <- subset(cur.plot, left.ear==j)
#    before <- subset(cur.rat, time=="before")
#    after <- subset(cur.rat, time=="after")
#    before.weight <- mean(before$weight, na.rm=TRUE)
#    after.weight <- mean(after$weight, na.rm=TRUE)
#    diff <- after.weight - before.weight
#    weight.diffs <- rbind(weight.diffs, 
#                          data.frame(i, j, unique(cur.rat$treatment), 
#                                                  before.weight, after.weight))  
    weight <- mean(cur.rat$weight, na.rm=TRUE)
    weight.diffs <- rbind(weight.diffs, data.frame(i, j, unique(cur.rat$treatment),
                                                   cur.rat$time[1], weight, cur.rat$sex[1], cur.rat$site[1]))
  }
}
#colnames(weight.diffs) <- c("plot", "rat", "treatment", "before", "after")
colnames(weight.diffs) <- c("plot", "rat", "treatment", "time", "weight", "sex", "site")
weight.aov.full <- lm(weight ~ treatment*time + sex, data=weight.diffs)
summary(weight.aov.full)
AIC(weight.aov.full)

weight.cpna <- subset(weight.diffs, site=="cpna")
weight.lok <- subset(weight.diffs, site=="lokern")
weight.czo <- subset(weight.diffs, site=="carrizo")

weight.aov.cpna <- lm(weight ~ treatment*time + sex, data=weight.cpna)
summary(weight.aov.cpna)

weight.aov.lok <- lm(weight ~ treatment*time + sex, data=weight.lok)
summary(weight.aov.lok)

weight.aov.czo <- lm(weight ~ treatment*time + sex, data=weight.czo)
summary(weight.aov.czo)

weight.diffs$time <- with(weight.diffs, factor(time, levels=rev(levels(time))))
ggplot(weight.diffs, aes(factor(treatment), weight)) + geom_boxplot(aes(fill=time)) +
  theme_bw() + scale_fill_grey(start=0.5, end=0) +
  labs(x="", y="Weight (g)") + facet_wrap(~site) +
  theme(legend.title=element_blank())

weight.treatment <- lm(weight ~ treatment + sex, data=weight.diffs)
AIC(weight.treatment)

weight.treatment.time <- lm(weight ~ treatment + time + sex, data=weight.diffs)
AIC(weight.treatment.time)

boxplot(weight.diffs$weight ~ weight.diffs$treatment + weight.diffs$time)


### Reproductive status
gkr.data <- subset(gkr.data, type=="N" | type=="S")
before <- subset(gkr.data, time=="before")
after <- subset(gkr.data, time=="after")
before.test <- glm(repro2 ~ treatment*site, data=before, family="binomial")
after.test <- glm(repro2 ~ treatment*site, data=after, family="binomial")

exp(cbind(OR = coef(before.test), confint(before.test)))
exp(cbind(OR = coef(after.test), confint(after.test)))



#######
# 3. Create unique capture histories for export
#######


### Capture history for combined data
unique.indivs <- subset(my.data, type == "N")

gkr <- subset(my.data, species == "DIIN")

all.cap.hist <- NULL

for(k in unique(gkr$time)){
  cur.time <- subset(gkr, time == k)
  for(l in unique(gkr$plot)){
    cur.plot <- subset(cur.time, plot==l)
    for(i in unique(cur.plot$left.ear)){
      cur.gkr <- subset(cur.plot, left.ear == i)
      cur.trap.hist <- NULL
      for(j in 1:5){
        if(j %in% unique(cur.gkr$night)){
          cur.trap.hist <- paste(cur.trap.hist, "1", sep="")
        } else {
          if(j > 3 && as.character(unique(cur.gkr$plot)) %in% c("1", "2", "4", "S1", "S2", "S3")){
            cur.trap.hist <- paste(cur.trap.hist, ".", sep="")
          } else {
          cur.trap.hist <- paste(cur.trap.hist, "0", sep="")
          }
        }
      }
      cur.rat <- data.frame(paste(substr(k,1,1), substr(unique(cur.gkr$treatment),1,1),
                                unique(cur.gkr$plot), sep=""), i, cur.trap.hist)
      all.cap.hist <- rbind(all.cap.hist, cur.rat)
    }
  }
}
colnames(all.cap.hist) <- c("plot", "indiv", "hist")

# Generate table of counts of different recapture histories
temp <- as.data.frame.matrix(table(all.cap.hist$hist, all.cap.hist$plot))
temp$cap.hist <- row.names(temp)
mark.inp <- data.frame(temp$cap.hist, temp[,1:32])

# alternate attempts at creating a summary table
#sum.hist.16 <- gather(temp, cap.hist, site, IVC:P89E)
#colnames(sum.hist.16) <- c("cap.hist", "site", "num")

# Save output
head.of.file <- paste(colnames(mark.inp), collapse=" ")
head.of.file <- paste("/*", head.of.file, "*/")
mark.out <- apply(mark.inp, 1, paste, collapse=" ")
mark.out <- paste(mark.out, ";", sep="")
mark.out <- c(head.of.file, mark.out)
fileconn <- file("../Analysis/all-data.inp")
writeLines(mark.out, fileconn)
close(fileconn)






for(i in 1:nrow(cur.data)){
  cur.rat <- cur.data[i,]$left.ear
  other.data <- cur.data[-i,]
  if(cur.rat %in% other.data$left.ear){
    print(cur.rat)
  }
}

for(i in unique(gkr$left.ear)){
  cur.rat <- subset(gkr, left.ear==i)
  if(length(unique(cur.rat$plot)) > 1){
    print(paste("Sites:", unique(cur.rat$site), "Rat:", i))
  }
}

for(i in unique(all.cap.hist$indiv)){
  cur.rat <- subset(all.cap.hist, indiv == i)
  if(nrow(cur.rat) > 1){
    print(cur.rat)
  }
}
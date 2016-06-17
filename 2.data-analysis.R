### GKR Preliminary Data Analysis

library(ggplot2)
library(plyr)
library(tidyr)
library(reshape)

#######
# 1. Load and clean data
#######

### Load all Lokern data
my.data <- read.csv("../Data/supp-data-combined.csv")
my.data$weight <- my.data$total.weight - my.data$bag.weight

#######
# 2. Visualize prelim results (total captured at each site, movement, body weight,
#        repro success)
#######

# Subtract minimun number of alive across years/plots...
mna <- subset(my.data, type=="N")
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

ggplot(diff.data, aes(factor(species), diffs)) + geom_point(stat="identity")

boxplot(diff.data$diffs ~ diff.data$species)

ggplot(mna, aes(factor(time), weight)) + geom_point() +
  facet_wrap(~treatment + species)

### Compare weights betwen treatment/control for GKR
gkr.data <- subset(my.data, species=="DIIN")
gkr.data <- subset(gkr.data, age=="A")

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
                                                   cur.rat$time[1], weight))
  }
}
#colnames(weight.diffs) <- c("plot", "rat", "treatment", "before", "after")
colnames(weight.diffs) <- c("plot", "rat", "treatment", "time", "weight")
weight.aov <- lm(weight ~ treatment*time, data=weight.diffs)
summary(weight.aov)
boxplot(weight.diffs$weight ~ weight.diffs$treatment + weight.diffs$time)


### Reproductive status
gkr.data <- subset(gkr.data, type=="N" | type=="S")
before <- subset(gkr.data, time=="before")
after <- subset(gkr.data, time=="after")
before.test <- glm(repro2 ~ treatment, data=before, family="binomial")
after.test <- glm(repro2 ~ treatment, data=after, family="binomial")

# Plot unique inidividual results by species from 2015
mna.before <- subset(my.data, type == "N")
mna.before <- subset(my.data, time == "before")
n.plot <- ggplot(mna.before, aes(factor(plot)))
n.plot + geom_bar() + facet_wrap(~species) 
                                            

             
# Plot unique inidividual results by species from 2016
unique.lokern <- subset(lokern, type == "N")
n.16.plot <- ggplot(unique.lokern, aes(factor(plot)))
n.16.plot + geom_bar() + facet_wrap(~species)

sum.15 <- as.data.frame.matrix(table(unique.lokern$plot, unique.lokern$species))
sum.15$plot <- row.names(sum.15)
sum.15$year <- rep(2015, nrow(sum.15))
sum.16 <- as.data.frame.matrix(table(unique.lokern$plot, unique.lokern$species))
sum.16$plot <- row.names(sum.16)
sum.16$year <- rep(2016, nrow(sum.16))

ses.recaps <- subset(lokern, type ==  "S")

## See if anyone moved
gkr.2015 <- subset(lokern, species == "DIIN")
gkr.2016 <- subset(lokern, species == "DIIN")

for(i in unique(ses.recaps$left.ear)){
  cur.gkr <- subset(lokern, left.ear == i)
  cur.gkr.16 <- subset(lokern, left.ear == i)
  print(paste("Left Ear:", i, "2015 Plots:", unique(cur.gkr$plot), "2016 Plots:", 
              unique(cur.gkr.16$plot)))
}


#######
# 3. Create unique capture histories for export
#######

# 2015
all.cap.hist.15 <- NULL

for(i in unique(gkr.2015$left.ear)){
  cur.gkr <- subset(gkr.2015, left.ear == i)
  cur.trap.hist <- NULL
  for(j in 1:5){
    if(j %in% unique(cur.gkr$night)){
      cur.trap.hist <- paste(cur.trap.hist, "1", sep="")    
    } else {
      cur.trap.hist <- paste(cur.trap.hist, "0", sep="")
    }
  }
  cur.rat <- data.frame(unique(cur.gkr$plot), i, cur.trap.hist)
  all.cap.hist.15 <- rbind(all.cap.hist.15, cur.rat)
}
colnames(all.cap.hist.15) <- c("plot", "indiv", "hist")

# Generate table of counts of different recapture histories for 2015
#sum.cap.hist.15 <- count(all.cap.hist.15$plot, all.cap.hist.15$hist)
temp <- as.data.frame.matrix(table(all.cap.hist.15$hist, all.cap.hist.15$plot))
temp$cap.hist <- row.names(temp)
#sum.hist.15 <- gather(temp, cap.hist, site, IVC:P89E)
#colnames(sum.hist.15) <- c("cap.hist", "site", "num")
#spread.hist <- spread(sum.hist.15, site, num)

mark.inp <- data.frame(temp$cap.hist, temp[,1:6])

# alternate attempts at creating a summary table
#sum.hist.16 <- gather(temp, cap.hist, site, IVC:P89E)
#colnames(sum.hist.16) <- c("cap.hist", "site", "num")

# Save output
mark.out.15 <- apply(mark.inp, 1, paste, collapse=" ")
mark.out.15 <- paste(mark.out.15, ";", sep="")
fileconn <- file("../Analysis/cnpa15.inp")
writeLines(mark.out.15, fileconn)
close(fileconn)

# 2016
all.cap.hist.16 <- NULL

for(i in unique(gkr.2016$left.ear)){
  cur.gkr <- subset(gkr.2016, left.ear == i)
  cur.trap.hist <- NULL
  for(j in 1:5){
    if(j %in% unique(cur.gkr$night)){
      cur.trap.hist <- paste(cur.trap.hist, "1", sep="")    
    } else {
      cur.trap.hist <- paste(cur.trap.hist, "0", sep="")
    }
  }
  cur.rat <- data.frame(unique(cur.gkr$plot), i, cur.trap.hist)
  all.cap.hist.16 <- rbind(all.cap.hist.16, cur.rat)
}
colnames(all.cap.hist.16) <- c("plot", "indiv", "hist")

# Generate table of counts of different recapture histories for 2016
temp <- as.data.frame.matrix(table(all.cap.hist.16$hist, all.cap.hist.16$plot))
temp$cap.hist <- row.names(temp)
mark.inp <- data.frame(temp$cap.hist, temp[,1:6])

# alternate attempts at creating a summary table
#sum.hist.16 <- gather(temp, cap.hist, site, IVC:P89E)
#colnames(sum.hist.16) <- c("cap.hist", "site", "num")

# Save output
mark.out.16 <- apply(mark.inp, 1, paste, collapse=" ")
mark.out.16 <- paste(mark.out.16, ";", sep="")
fileconn <- file("../Analysis/cnpa16.inp")
writeLines(mark.out.16, fileconn)
close(fileconn)


## Create Robust Design Capture History
gkr.16.temp <- data.frame(gkr.2016$date, gkr.2016$night, gkr.2016$plot,
                          gkr.2016$trap, gkr.2016$left.ear)
gkr.16.temp$ses <- rep(2, nrow(gkr.16.temp))
colnames(gkr.16.temp) <- c("date", "night", "plot", "trap", "left.ear", "session")

gkr.15.temp <- data.frame(gkr.2015$date, gkr.2015$night, gkr.2015$plot,
                          gkr.2015$trap, gkr.2015$left.ear)
gkr.15.temp$ses <- rep(1, nrow(gkr.15.temp))
colnames(gkr.15.temp) <- c("date", "night", "plot", "trap", "left.ear", "session")

combined.data <- rbind(gkr.15.temp, gkr.16.temp)

comb.cap.hist <- NULL

for(i in unique(combined.data$left.ear)){
  cur.gkr <- subset(combined.data, left.ear == i)
  cur.trap.hist <- NULL
  cur.ses <- subset(cur.gkr, session==1)
  for(j in 1:5){
    if(j %in% unique(cur.ses$night)){
      cur.trap.hist <- paste(cur.trap.hist, "1", sep="")
    } else {
      cur.trap.hist <- paste(cur.trap.hist, "0", sep="")
    }
  }
  
  cur.ses <- subset(cur.gkr, session==2)
  for(j in 1:5){
    if(j %in% unique(cur.ses$night)){
      cur.trap.hist <- paste(cur.trap.hist, "1", sep="")    
    } else {
      cur.trap.hist <- paste(cur.trap.hist, "0", sep="")
    }
  }
  cur.rat <- data.frame(unique(cur.gkr$plot), i, cur.trap.hist)
  comb.cap.hist <- rbind(comb.cap.hist, cur.rat)
}
colnames(comb.cap.hist) <- c("plot", "indiv", "hist")
temp <- as.data.frame.matrix(table(comb.cap.hist$hist, comb.cap.hist$plot))
temp$cap.hist <- row.names(temp)
mark.inp <- data.frame(temp$cap.hist, temp[,1:6])

# Save output
mark.out.comb <- apply(mark.inp, 1, paste, collapse=" ")
mark.out.comb <- paste(mark.out.comb, ";", sep="")
fileconn <- file("../Analysis/cnpa-robust.inp")
writeLines(mark.out.comb, fileconn)
close(fileconn)

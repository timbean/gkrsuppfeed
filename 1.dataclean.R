### GKR Preliminary Data Analysis

library(ggplot2)
library(plyr)
library(tidyr)

#######
# 1. Load and clean data
#######

### Load 2015 data
cpna.2015 <- read.csv("../Data/CPNA/griddata2015.csv")

# Rename 2015 sites
old.site.names <- unique(cpna.2015$ident)
new.site.names <- c("P88E", "P88C", "P89C", "P89E", "IVC", "IVE")
site.lookup <- data.frame(old.site.names, new.site.names)
colnames(site.lookup) <- c("ident", "plot")
cpna.2015 <- merge(cpna.2015, site.lookup)

# Re-code species
type <- unique(cpna.2015$type)
sp.codes <- c("PEIN", "NA", "DIIN", "PEIN", "DIIN", "NA", "NA", "PEIN", "NA",
              "ONTO", "DIHE", "NA", "AMNE")
type.codes <- c("N", "T", "N", "R", "R", "T", "NA", "R", "NA", "N", "N", "T", "N")
code.lookup <- data.frame(type, sp.codes, type.codes)
cpna.2015 <- merge(cpna.2015, code.lookup)

cpna.2015$species <- cpna.2015$sp.codes
cpna.2015$type <- cpna.2015$type.codes

### Load 2016 data
cpna.2016 <- read.csv("../Data/CPNA/griddata2016.csv")
cpna.2016$plot <- as.character(cpna.2016$plot)
cpna.2016 <- subset(cpna.2016, plot != "176")
cpna.2016 <- subset(cpna.2016, plot != "P89")
cpna.2016$plot <- factor(cpna.2016$plot)

# Re-code species
type <- unique(cpna.2016$type)
species <- unique(cpna.2016$species)


#######
# 2. Visualize prelim results (total captured at each site, movement, body weight,
#        repro success)
#######

# Plot unique inidividual results by species from 2015
unique.cpna.2015 <- subset(cpna.2015, type == "N")
n.15.plot <- ggplot(unique.cpna.2015, aes(factor(plot)))
n.15.plot + geom_bar() + facet_wrap(~species) 
                                                         
# Plot unique inidividual results by species from 2016
unique.cpna.2016 <- subset(cpna.2016, type == "N")
n.16.plot <- ggplot(unique.cpna.2016, aes(factor(plot)))
n.16.plot + geom_bar() + facet_wrap(~species)

sum.15 <- as.data.frame.matrix(table(unique.cpna.2015$plot, unique.cpna.2015$species))
sum.15$plot <- row.names(sum.15)
sum.15$year <- rep(2015, nrow(sum.15))
sum.16 <- as.data.frame.matrix(table(unique.cpna.2016$plot, unique.cpna.2016$species))
sum.16$plot <- row.names(sum.16)
sum.16$year <- rep(2016, nrow(sum.16))

ses.recaps <- subset(cpna.2016, type ==  "S")

## See if anyone moved
gkr.2015 <- subset(cpna.2015, species == "DIIN")
gkr.2016 <- subset(cpna.2016, species == "DIIN")

for(i in unique(ses.recaps$left.ear)){
  cur.gkr <- subset(cpna.2015, left.ear == i)
  cur.gkr.16 <- subset(cpna.2015, left.ear == i)
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
sum.hist.15 <- gather(temp, cap.hist, site, IVC:P89E)
colnames(sum.hist.15) <- c("cap.hist", "site", "num")
spread.hist <- spread(sum.hist.15, site, num)

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
mark.inp <- temp

# alternate attempts at creating a summary table
#sum.hist.16 <- gather(temp, cap.hist, site, IVC:P89E)
#colnames(sum.hist.16) <- c("cap.hist", "site", "num")






### GKR Preliminar Data Analysis

#######
# 1. Load and clean data
#######

### Load 2015 data
cpna.2015 <- read.csv("../Data/CPNA/griddata2015.csv")

# Rename 2015 sites
old.site.names <- unique(cpna.2015$ident)
new.site.names <- c("88E", "88C", "89C", "89E", "IVC", "IVE")
site.lookup <- data.frame(old.site.names, new.site.names)
colnames(site.lookup) <- c("ident", "plot")
cpna.2015 <- merge(cpna.2015, site.lookup)

# Re-code species
type <- unique(cpna.2015$type)
sp.codes <- c("SJPM", "NA", "DIIN", "SJPM", "DIIN", "NA", "NA", "SJPM", "NA",
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

#######
# 2. Visualize prelim results (total captured at each site, movement, body weight,
#        repro success)
#######



#######
# 3. Create unique capture histories for export
#######

## July 9, 2015
## Author: Bastiaen Boekelo
## Goal: Inspecting frequency of bee species records where multiple_year records for same species have been deleted first.


rm(list=ls())


Source_loc <- "C:/Users/BranderPC/Dropbox/WUR/Thesis GIS/Data/BeeData_Adjusted/South_from_2000_WithoutDoubles_for_R.csv"

Bees_all <- read.csv(Source_loc, header=T, sep=";")
Bees_all <- Bees_all[,1:9]
Bees_from2002 <- subset(Bees_all, JAAR >2001)
Bees_from2003 <- subset(Bees_all, JAAR >2002)
Bees_from2004 <- subset(Bees_all, JAAR >2003)
Bees_from2005 <- subset(Bees_all, JAAR >2004)

species <- unique(Bees_all$FULL_SPECIES)

all_recs_sp_only <- Bees_all$FULL_SPECIES

#### Creating species frequency table
sp_freq <- data.frame(Species=character(), Counts=integer())
for(i in 1:length(species)){
  newdata <- subset(Bees_from2003, FULL_SPECIES == species[i]) # Enter the YEAR_selection here
  counts <- nrow(newdata)
  sp_row <- matrix(c(as.character(species[i]),counts), nrow=1)
  sp_freq <- rbind(sp_freq, sp_row)
}
colnames(sp_freq) <- c("Species", "Counts")
sp_freq$Counts <- as.numeric(as.character(sp_freq$Counts))
######


# Creating selection of species with minumum record
subset_100 <- subset(sp_freq, Counts > 100)
subset_sp_100 <- as.vector(sp_freq$Species[sp_freq[2] > 100])
subset_counts_100 <- as.vector(sp_freq$Counts[sp_freq[2] > 100])
nrow(subset_100)

# Creating SUBSETTED TABLE   - - - !! CHECK year and species treshold in name !!
Bees_subset_2003_100 <- subset(Bees_from2003, FULL_SPECIES == subset_sp_100[1])
for(i in 2:length(subset_sp_100)){
  newdata <- subset(Bees_from2003, FULL_SPECIES == subset_sp_100[i]) # Enter the YEAR_selection here
  Bees_subset_2003_100 <- rbind(Bees_subset_2003_100, newdata)
}
Bees_subset_2003_100 <- droplevels(Bees_subset_2003_100)








## July 9, 2015
## Author: Bastiaen Boekelo
## Goal: Inspecting frequency of bee species records where multiple_year records for same species have been deleted first.


rm(list=ls())

Source_loc <- "data/Bee_data/FOR_R_origin_from_Bee_data_Prepared_for_R.csv"

# Subsetting South of the Netherlands and relevant columns

Bees_all <- read.csv(Source_loc, header=T, sep=";")
Bees_all <- subset(Bees_all, Y_Round < 475)
Bees_all <- Bees_all[,c(1,2,3,4,13)]
species <- unique(Bees_all$FULL_SPECIES)

############ Creating species frequency table #############
sp_freq <- data.frame(Species=character(), Counts=integer())
for(i in 1:length(species)){
  newdata <- subset(Bees_all, FULL_SPECIES == species[i]) 
  counts <- nrow(newdata)
  sp_row <- matrix(c(as.character(species[i]),counts), nrow=1)
  sp_freq <- rbind(sp_freq, sp_row)
}
rm(sp_row)
colnames(sp_freq) <- c("Species", "Counts")
sp_freq$Counts <- as.numeric(as.character(sp_freq$Counts))
############################################################

# Subsetting the South part of the Netherlands

# Creating selection of species with minumum record
subset_100 <- subset(sp_freq, Counts > 100)
subset_sp_100 <- as.vector(sp_freq$Species[sp_freq[2] > 100])
subset_counts_100 <- as.vector(sp_freq$Counts[sp_freq[2] > 100])
nrow(subset_100)

# Creating SUBSETTED TABLE   - - - !! CHECK year and species treshold in name !!
Bees_subset_2003_100 <- subset(Bees_all, FULL_SPECIES == subset_sp_100[1])
for(i in 2:length(subset_sp_100)){
  newdata <- subset(Bees_all, FULL_SPECIES == subset_sp_100[i]) # Enter the YEAR_selection here
  Bees_subset_2003_100 <- rbind(Bees_subset_2003_100, newdata)
}
rm(newdata)

# Write to table
Bees_subset_2003_100 <- droplevels(Bees_subset_2003_100)
write.table(Bees_subset_2003_100, "data/Bee_data/Bee_data_SDM_input.csv")







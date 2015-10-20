## July 9, 2015
## Author: Bastiaen Boekelo
## Goal: Inspecting frequency of bee species records where multiple_year records for same species have been deleted first.


rm(list=ls())
setwd("D:/R_Projects/Thesis_GRS2")
Source_loc <- "data/Bee_data/FOR_R_origin_from_Bee_data_Prepared_for_R.csv"

# Subsetting South of the Netherlands and relevant columns

Bees_all <- read.csv(Source_loc, header=T, sep=";")
Bees_all <- subset(Bees_all, Y_Round < 475)
Bees_all <- Bees_all[,c(1,2,3,4,11:13)]
species <- unique(Bees_all$FULL_SPECIES)
names(Bees_all)

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

# Write species table to csv
subset_100$Genus_name <- NA
subset_100$Species_name <- NA
for(i in 1:nrow(subset_100)){
  full_sp <- as.character(subset_100[i,1])
  subset_100[i,3] <- as.character(as.data.frame(strsplit(full_sp, "_"))[1,1])
  subset_100[i,4] <- as.character(as.data.frame(strsplit(full_sp, "_"))[2,1])
}
write.table(subset_100, "D:/SDM/Bee_data.txt", sep=" ", row.names =F)

# Creating SUBSETTED TABLE   - - - !! CHECK year and species treshold in name !!
Bees_subset_2003_100 <- subset(Bees_all, FULL_SPECIES == subset_sp_100[1])
for(i in 2:length(subset_sp_100)){
  newdata <- subset(Bees_all, FULL_SPECIES == subset_sp_100[i]) # Enter the YEAR_selection here
  Bees_subset_2003_100 <- rbind(Bees_subset_2003_100, newdata)
}
rm(newdata)

length(unique(Bees_subset_2003_100$X_Y_COOR)) # Number of unique records

# Assign new coordinates for the conversion of the point data to raster in ArcGIS
Bees_subset_2003_100$X_ArcGIS <- 1
Bees_subset_2003_100$Y_ArcGIS <- 1
for(i in 1:nrow(Bees_subset_2003_100)){
  Bees_subset_2003_100$X_ArcGIS[i] <- Bees_subset_2003_100$X_Round[i] * 1000 + 500
  Bees_subset_2003_100$Y_ArcGIS[i] <- Bees_subset_2003_100$Y_Round[i] * 1000 + 500
}

hist(subset_100$Counts, breaks=15)

# Extracting number of species per location (using unique coordinates)
Coor_freq <- as.data.frame(table(Bees_subset_2003_100$X_Y_COOR))
Sp_div_obs <- merge(Coor_freq, Bees_subset_2003_100, by.x="Var1", by.y="X_Y_COOR")
Sp_div_obs <- data.frame(Sp_div_obs$X_ArcGIS, Sp_div_obs$Y_ArcGIS, Sp_div_obs$Freq)
Sp_div_obs <- unique(Sp_div_obs)
write.table(Sp_div_obs, "data/Bee_data/Nr_of_sp_per_location.csv", sep=",", row.names=F)

# Write to table
Bees_subset_2003_100 <- droplevels(Bees_subset_2003_100)
write.table(Bees_subset_2003_100, "data/Bee_data/Bee_data_All_sp_1_column.csv", row.names=F)

# Continuing with making data for species in separate columns
FULL_SP_table <- Bees_subset_2003_100
species <- unique(FULL_SP_table[4])

# Creating separate presence column for every species
for(i in 1:nrow(species)){
  name_column <- as.character(species[i,])
  FULL_SP_table[,name_column] <- NA
  for(ii in 1:nrow(FULL_SP_table)){
    if(FULL_SP_table[ii,4]==name_column){
      FULL_SP_table[ii,name_column] <- 1
    } 
  } 
}

nr_locations <- unique(Bees_subset_2003_100$X_Y_COOR)

# Write to table
write.table(FULL_SP_table, "data/Bee_data/Bee_data_SDM_input_All_sp_sep_columns.csv", row.names=F)




##########################################################################
#### SOME ARCGIS ANALYSES --> RASTER OF LOCATIONS WITH AT LEAST 1 SPECIES
##########################################################################

# Then, read that table

# Bees_presence <- raster("data/Bee_data/Bees_raster_min_1_loc/At_Least_one_record.tif")
# plot(Bees_presence)

##################################################
##################################################





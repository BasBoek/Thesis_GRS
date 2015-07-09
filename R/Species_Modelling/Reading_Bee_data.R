rm(list=ls())


Source_loc <- "C:/Users/BranderPC/Dropbox/WUR/Thesis GIS/Data/BeeData_Adjusted/South_from_2000_WithoutDoubles_for_R.csv"

Bees_all <- read.csv(Source_loc, header=T, sep=";")
Bees_all <- Bees_all[,1:9]
Bees_from2002 <- subset(Bees_all, JAAR >2001)
Bees_from2003 <- subset(Bees_all, JAAR >2002)
Bees_from2004 <- subset(Bees_all, JAAR >2003)

species <- unique(Bees_all$FULL_SPECIES)

all_recs_sp_only <- Bees_all$FULL_SPECIES

sp_freq <- data.frame(Species=character(), Counts=integer())
for(i in 1:length(species)){
  newdata <- subset(Bees_from2004, FULL_SPECIES == species[i])
  counts <- nrow(newdata)
  sp_row <- matrix(c(as.character(species[i]),counts), nrow=1)
  sp_freq <- rbind(sp_freq, sp_row)
}
for(ii in sp_freq[,1]){
  sp_freq[ii,2] <- as.numeric(sp_freq[ii,2])
}

sp_freq[sp_freq$Counts > 100]

colnames(sp_freq) <- c("Species", "Counts")
sp_freq[,1]

All_records <- do.call("rbind", read.table, header = F))
sp_row


## Author:  Bastiaen Boekelo
## Date:    07-06-2015
## Content: Creates non-spatial landscape variable table of every km2 of the Netherlands

rm(list=ls())
library(raster)
library(SDMTools)
source("R/Create_Input_Variables/List_KM_Rasters.R")

##########################################
########## BEE_SUITABILITY  ############
##########################################

# SET SOURCE & DESTINATION LOCATION (and listing rasters)
workspace_source <- "D:/SDM/Input_Rasters/ZZSplit_Rasters/_Split_Land_Use/LGN6_BeeSuitability/"
workspace_dest <- "D:/SDM/Input_Rasters/ZZSplit_Rasters/Split_Tables/BeeSuitability/"
List_KM_Rasters(workspace_source)

# Set Name of final table
table_name <- "_BeeSuitability"

# DETERMINE NA VALUES!
NA_val <- 128

for(i in 1:length(raslist)){
  ras<-raster(raslist[i])
  b<-ClassStat(ras,25) # 25 = cell size
  b$Area_total_Km2_BS <-sum(as.numeric(b[b$class!=NA_val,3]))/1000000 #Km2
  ras[ras==128] <- NA
  b$Suitability <- cellStats((ras-10), 'mean')*10
  filename<-as.character(names(ras))
  b$landscape<-filename  	#CAPTURE THE LANDSCAPE NUMBER I THE ROW
  last_column <- length(colnames(b))
  b<-b[1,c(39:last_column)]	#EXTRACT ONLY IMPORTANT COLUMNS
  write.table(b,paste(workspace_dest, filename, ".txt", sep=""), row.names = F)
  print(paste("img",i,"of", length(raslist)))
}
print("done!")

tablist <- list.files(workspace_dest, pattern ="*.txt", full.names=TRUE)
All_records <- do.call("rbind", lapply(tablist, read.table, header = TRUE))
write.csv(All_records, paste(workspace_dest, table_name,".csv", sep=""))

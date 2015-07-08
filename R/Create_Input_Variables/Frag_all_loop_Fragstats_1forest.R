## Author:  Bastiaen Boekelo
## Date:    07-06-2015
## Content: Creates non-spatial landscape variable table of every km2 of the Netherlands

rm(list=ls())
library(raster)
library(SDMTools)
source("R/Create_Input_Variables/List_KM_Rasters.R")

##########################################
########## FRAGSTATS_1FOREST  ############
##########################################

# SET SOURCE & DESTINATION LOCATION (and listing rasters)
workspace_source <- "D:/SDM/Input_Rasters/ZZSplit_Rasters/_Split_Land_Use/LGN6_Fragstats_1forest/"
workspace_dest <- "D:/SDM/Input_Rasters/ZZSplit_Rasters/Split_Tables/Fragstats_1forest/"
List_KM_Rasters(workspace_source)

# Set Name of final table
table_name <- "_Fragstats_1forest"

# DETERMINE NA VALUES!
NA_val <- 9999

for(i in 1:length(raslist)){
  ras<-raster(raslist[i])
  b<-ClassStat(ras,25) # 25 = cell size
  b$NumClasses_Fragstat <-sum(as.numeric(b[,1]!=NA_val))#CALCULATE NUMBER OF CLASSES TAKING NODATA AWAY
  b$ED_total_Fragstat <-sum(as.numeric(b[b$class!=NA_val,7]))#SUM ED (SUM ED OF DIFFERENT CLASSES)
  b$Area_total_Km2_Frag_1For <-sum(as.numeric(b[b$class!=NA_val,3]))/1000000 #Km2
  filename<-as.character(names(ras))
  b$landscape<-filename		#CAPTURE THE LANDSCAPE NUMBER I THE ROW
  last_column <- length(colnames(b))
  b<-b[1,c(39:last_column)]	#EXTRACT ONLY IMPORTANT COLUMNS
  write.table(b,paste(workspace_dest, filename, ".txt", sep=""), row.names = F)
  print(paste("img",i,"of", length(raslist)))
}
print("done!")

tablist <- list.files(workspace_dest, pattern ="*.txt", full.names=TRUE)
All_records <- do.call("rbind", lapply(tablist, read.table, header = TRUE))
write.csv(All_records, paste(workspace_dest, table_name,".csv", sep=""))

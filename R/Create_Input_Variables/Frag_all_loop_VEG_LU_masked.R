## Author:  Bastiaen Boekelo
## Date:    07-06-2015
## Content: Creates non-spatial landscape variable table of every km2 of the Netherlands

rm(list=ls())
library(raster)
library(SDMTools)
source("R/Create_Input_Variables/List_KM_Rasters.R")

##########################################
########## VEG_LU_Masked  ############
##########################################

# SET SOURCE & DESTINATION LOCATION (and listing rasters)
workspace_source <- "D:/SDM/Input_Rasters/ZZSplit_Rasters/_Split_Vegetation_and_LU/Veg_LU_masked/"
workspace_dest <- "D:/SDM/Input_Rasters/ZZSplit_Rasters/Split_Tables/VEG_LU_Masked/"
List_KM_Rasters(workspace_source)

# Set Name of final table
table_name <- "_VEG_LU_Masked"

# DETERMINE NA VALUES!
NA_val <- 128

# What are the unique names of the classes?
uniques <- c(0,1,2,3,4,5,6,7,8,20,30,60)

for(i in 1:length(raslist)){
  ras<-raster(raslist[i])
  b<-ClassStat(ras,25) # 25 = cell size
  b$NumClasses_VEG_LU <-sum(as.numeric(b[,1]!=NA_val))#CALCULATE NUMBER OF CLASSES TAKING NODATA AWAY
  b$Mean_Patch_areaKm2_Veg_LU <-sum(as.numeric(b[b$class!=NA_val,10]))/1000000#km2
  b$Area_total_Km2_VEG_LU <-sum(as.numeric(b[b$class!=NA_val,3]))/1000000 #Km2
  filename<-as.character(names(ras))
  b$landscape<-filename    #CAPTURE THE LANDSCAPE NUMBER I THE ROW
  
  ##########################################################################
  ## CREATING NEW COLUMNS WITH '% AREA OF TOTAL' PER CLASS
  for(ii in uniques){
    Area <- b$total.area[b$class==ii]
    empty_check <- length(Area)
    if(empty_check==0){
      b$var <- 0
    } else {
      b$var <- (b$total.area[b$class==ii] / 1000000) / b$Area_total_Km2_VEG_LU * 100
    }
    New_name <- paste("LUVEG_", as.character(ii), sep="")
    last_column <- length(colnames(b))
    colnames(b)[last_column] <- New_name
  }
  ###########################################################################
  
  b<-b[1,c(39:last_column)]	#EXTRACT ONLY IMPORTANT COLUMNS
  write.table(b,paste(workspace_dest, filename, ".txt", sep=""), row.names = F)
  print(paste("img",i,"of", length(raslist)))
}
print("done!")

tablist <- list.files(workspace_dest, pattern ="*.txt", full.names=TRUE)
All_records <- do.call("rbind", lapply(tablist, read.table, header = TRUE))
write.csv(All_records, paste(workspace_dest, table_name,".csv", sep=""))




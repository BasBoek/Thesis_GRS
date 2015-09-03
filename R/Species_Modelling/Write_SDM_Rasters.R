# Author: Bastiaen Boekelo
# Date last edit: September 3, 2015
# Goal: Reads .gri rasters and writes them into one final SDM raster folder

Write_SDM_Rasters <- function(myRespName, Input_Loc, Output_Loc){
  genus_name <- as.character(as.data.frame(strsplit(myRespName, "_"))[1,])
  species_name  <- as.character(as.data.frame(strsplit(myRespName, "_"))[2,])
  
  Rasters <- list.files(paste("D:/SDM/SDM_Output/", Input_Loc, "/", genus_name, ".", species_name, "/proj_current", sep=""), pattern=".gri", full.names=T)
  Rasternames <- list.files(paste("D:/SDM/SDM_Output/", Input_Loc, "/", genus_name, ".", species_name, "/proj_current", sep=""), pattern=".gri")
  
  for(i in 1:length(Rasters)){
    RasName <- substring(Rasternames[i], 14, nchar(Rasternames[i])-4)
    RasName <- gsub("\\.", "_", RasName) # Added this line, because species_name =< 7 characters would result in wrong naming and writing
    SDM_ras <- stack(Rasters[i])
    writeRaster(SDM_ras, paste(Output_Loc, RasName, sep=""), "GTiff", overwrite=TRUE)    
  }
}



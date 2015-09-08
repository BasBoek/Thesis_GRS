# Author: Bastiaen Boekelo
# Last edit: September 7, 2015
# Goal, derive information about prediction certainty for final maps

library(raster)
library(rgdal)
rm(list=ls())

Vars <- c("LU")

for(Expl_Var in Vars){

  rasters <- list.files(paste("D:/SDM/SDM_Output/", Expl_Var, "/_Rasters_SDM_Models/", sep=""))
  ROI <- rasters[!grepl("ensemble", rasters) & !grepl("ROCbin", rasters)]
  ROCbins <- rasters[!grepl("ensemble", rasters) & grepl("ROCbin", rasters)]
  
  CV_lijst <- data.frame(Species=character(), CV=numeric(), stringsAsFactors=FALSE) 
  
  for(i in 1:length(ROI)){ 
    # Derive products form the 30 individual SDMs (in this case all GLMs)
    GLM_ras <- brick(paste("D:/SDM/SDM_Output/", Expl_Var, "/_Rasters_SDM_Models/", ROI[i], sep=""))
    Mean_rasters <- mean(GLM_ras)
    
    Mean_GLM <- calc(GLM_ras, fun=mean)
    
    VAR_GLM <- Mean_GLM
    for(ii in 1:nbands(GLM_ras)){
      new_ras <- (GLM_ras[[ii]]-Mean_GLM)^2
      VAR_GLM <- VAR_GLM + new_ras
    }
    writeRaster(VAR_GLM, paste("D:/SDM/SDM_SecondaryProducts/", Expl_Var, "/VAR_", ROI[i], sep=""), format="GTiff", overwrite=T)
    writeRaster(Mean_GLM, paste("D:/SDM/SDM_SecondaryProducts/", Expl_Var, "/Mean_", ROI[i], sep=""), format="GTiff", overwrite=T)
    
    SD_GLM <- sqrt((VAR_GLM - Mean_GLM)/(nbands(GLM_ras)))
    CV_GLM <- SD_GLM/Mean_GLM
    CV_GLMdf <- as.data.frame(CV_GLM)
    mean_CV <- mean(CV_GLMdf[,1], na.rm=T)
    
    CV_lijst[i,1] <- substring(ROI[i], 1, nchar(ROI[i])-4)
    CV_lijst[i,2] <- mean_CV
  }
  
  write.table(CV_lijst, file=paste("D:/SDM/SDM_SecondaryProducts/", Expl_Var, "/CV_list.csv", sep=""), row.names=F, sep=",")
}


# Derive the maps where in 90% of all cases a species occurence was predicted

for(Expl_Var in Vars){
  for(zz in 1:length(ROCbins)){
    Binaries <- brick(paste("D:/SDM/SDM_Output/", Expl_Var, "/_Rasters_SDM_Models/", ROCbins[zz], sep=""))
    Mean_bin <- mean(Binaries)
    Mean_bin[Mean_bin < 0.9] <- 0
    Mean_bin[Mean_bin >= 0.9] <- 1
    writeRaster(Mean_bin, paste("D:/SDM/SDM_SecondaryProducts/", Expl_Var, "/90pc_", substring(ROCbins[zz], 1, nchar(ROCbins[zz])-11), sep=""), format="GTiff", overwrite=T)
  } 
}



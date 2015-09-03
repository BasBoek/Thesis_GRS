library(raster)
rm(list=ls())

rasters <- list.files("D:/SDM/SDM_Output/_Results_Previous/LUVEG/_Rasters_SDM_Models/")
ROI <- rasters[!grepl("ensemble", rasters) & !grepl("ROCbin", rasters)]
CV_lijst <- data.frame(Species=character(), CV=numeric(), stringsAsFactors=FALSE) 

for(i in 1:length(ROI)){
  GLM_ras <- brick(paste("D:/SDM/SDM_Output/LUVEG/_Rasters_SDM_Models/", ROI[i], sep=""))
  Mean_rasters <- mean(GLM_ras)
  
  Mean_GLM <- calc(GLM_ras, fun=mean)
  
  VAR_GLM <- Mean_GLM
  for(ii in 1:nbands(GLM_ras)){
    new_ras <- (GLM_ras[[ii]]-Mean_GLM)^2
    VAR_GLM <- VAR_GLM + new_ras
  }
  SD_GLM <- sqrt((VAR_GLM - Mean_GLM)/(nbands(GLM_ras)))
  
  CV_GLM <- SD_GLM/Mean_GLM
  CV_GLMdf <- as.data.frame(CV_GLM)
  mean_CV <- mean(CV_GLMdf[,1], na.rm=T)
  
  CV_lijst[i,1] <- substring(ROI[i], 1, nchar(ROI[i])-4)
  CV_lijst[i,2] <- mean_CV
}
CV_lijst


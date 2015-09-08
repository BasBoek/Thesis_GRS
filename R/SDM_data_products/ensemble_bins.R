rm(list=ls())


VARS <- c("LU", "LUVEG", "VEG")

for(Expl_Var in VARS){
  
  rasters_ensbin <- list.files(paste("D:/SDM/SDM_Output/", Expl_Var, "/_Rasters_SDM_Models", sep=""), pattern = "ensemble_ROCbin", full.names=T)
  
  # Create sum of two rasters
  Ras_div <- raster(rasters_ensbin[1])
  for(i in 2:length(rasters_ensbin)){
    blieb <- as.data.frame(raster(rasters_ensbin[i]))
    blieb <- mean(blieb[[1]], na.rm=T)
    if(is.na(blieb) == F){
      print(i)
      new_ras <- raster(rasters_ensbin[i])
      Ras_div <- Ras_div + new_ras
    } else{
      print(rasters_ensbin[i])
    }
  }
  writeRaster(Ras_div, paste("D:/SDM/SDM_SecondaryProducts/Diversity_ens_", Expl_Var, sep=""), format="GTiff", overwrite=T)
}
plot(raster("D:/SDM/SDM_SecondaryProducts/Diversity_ens_VEG.tif"))




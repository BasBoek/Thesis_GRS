# Author: Bastiaen Boekelo
# September 7, 2015
# Goal: Create the 90% raster and the Sum of variation

rm(list=ls())

# List the rasters

VARS <- c("LU", "LUVEG", "VEG")

for(Expl_Var in VARS){

  rasters_90 <- list.files(paste("D:/SDM/SDM_SecondaryProducts/", Expl_Var, sep=""), pattern = "90", full.names=T)
  rasters_VAR <- list.files(paste("D:/SDM/SDM_SecondaryProducts/", Expl_Var, sep=""), pattern = "VAR", full.names=T)
  
  # Create sum of two rasters
  All_ras_90 <- raster(rasters_90[1])
  All_ras_VAR <- raster(rasters_VAR[1])
  for(i in 2:length(rasters_90)){
    blieb <- as.data.frame(raster(rasters_90[i]))
    blieb <- mean(blieb[[1]], na.rm=T)
    if(is.na(blieb) == F){
      print(i)
      new_ras_90 <- raster(rasters_90[i])
      All_ras_90 <- All_ras_90 + new_ras_90
      new_ras_VAR <- raster(rasters_VAR[i])
      All_ras_VAR <- All_ras_VAR + new_ras_VAR
    } else{
      print(rasters_90[i])
    }
  }
  writeRaster(All_ras_VAR, paste("D:/SDM/SDM_SecondaryProducts/All_ras_VAR_", Expl_Var, sep=""), format="GTiff", overwrite=T)
  writeRaster(All_ras_90, paste("D:/SDM/SDM_SecondaryProducts/All_ras_90_", Expl_Var, sep=""), format="GTiff", overwrite=T)
  
}

# These species are not included in LU:
# "D:/SDM/SDM_SecondaryProducts/LU/90pc_Andrena_haemorrhoa.tif"
# "D:/SDM/SDM_SecondaryProducts/LU/90pc_Bombus_lucorum.tif"
# "D:/SDM/SDM_SecondaryProducts/LU/90pc_Sphecodes_monilicornis.tif"

# These species are not included in LUVEG:
# [1] "D:/SDM/SDM_SecondaryProducts/LUVEG/90pc_Heriades_truncorum.tif"
# [1] "D:/SDM/SDM_SecondaryProducts/LUVEG/90pc_Hylaeus_communis.tif"

# These species are not included in VEG:
# [1] "D:/SDM/SDM_SecondaryProducts/VEG/90pc_Andrena_chrysosceles.tif"
# [1] "D:/SDM/SDM_SecondaryProducts/VEG/90pc_Bombus_lucorum.tif"
# [1] "D:/SDM/SDM_SecondaryProducts/VEG/90pc_Bombus_terrestris.tif"
# [1] "D:/SDM/SDM_SecondaryProducts/VEG/90pc_Hylaeus_confusus.tif"
# [1] "D:/SDM/SDM_SecondaryProducts/VEG/90pc_Lasioglossum_villosulum.tif"




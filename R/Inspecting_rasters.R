library(raster)

# Environmental variables
workspace_EV = "D:/SDM/Input_Rasters_KM2/full_area/LU/"
plot(raster(paste(workspace_EV, "Mean_Patch_areaKm2_BS.tif", sep="")))

#Names of environmental variables
ENV_LU <- list.files("D:/SDM/Input_Rasters_KM2/full_area/LU", pattern='tif$', full.names=T)
ENV_VEG <- list.files("D:/SDM/Input_Rasters_KM2/full_area/VEG", pattern='tif$', full.names=T)
ENV_LUVEG <- list.files("D:/SDM/Input_Rasters_KM2/full_area/LUVEG", pattern='tif$', full.names=T)

plot(raster(ENV_LU[8]))
plot(raster(ENV_VEG[7]))
plot(raster(ENV_LUVEG[2]))

print(ENV_LU)
print(ENV_LUVEG)
print(ENV_VEG)


## SDM projections

Bees_ROCbin <- list.files("D:/SDM/SDM_Output/LU/_Rasters_SDM_Models/", pattern='ROCbin',full.names=T)
Bees_ROCbin <- Bees_ROCbin[!grepl("ensemble", Bees_ROCbin)]

Bees_GLMs <- list.files("D:/SDM/SDM_Output/LU/_Rasters_SDM_Models/",full.names=T)
Bees_GLMs <- Bees_GLMs[!grepl("ensemble", Bees_GLMs)]
Bees_GLMs <- Bees_GLMs[!grepl("ROCbin", Bees_GLMs)]

Bees_ens_ROCbin <- list.files("D:/SDM/SDM_Output/LU/_Rasters_SDM_Models/", pattern='ensemble_ROCbin',full.names=T)

Bees_ens <- list.files("D:/SDM/SDM_Output/LU/_Rasters_SDM_Models/", pattern='ensemble.tif$',full.names=T)



plot(brick(Bees_ROCbin[1]))
plot(brick(Bees_GLMs[1]))

plot(raster(Bees_ens_ROCbin[1]))
plot(raster(Bees_ens[1]))













# Author: Bastiaen Boekelo
# Date: June 3, 2015
# Description: merge rasters

rm(list=ls())

library(raster)

## Listing stacked and projected rasters 
workspace <- "D:/Workspace_LiDAR/Rasters_25m"
files_c_ras <- paste(workspace, "/", list.files(workspace, pattern = "[c]_.*tif"), sep = "")
files_d_ras <- paste(workspace, "/" ,list.files(workspace, pattern = "[d]_.*tif"), sep = "")

## Merging them (for loop)

test <- raster(files_c_ras[3])

## Listing, loading and stacking
RD_new <- crs("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
nr_folders <- length(Dirlist)
for(i in 1:nr_folders){
  AHN_tile_full <- Dirlist_full[i]
  tile_name <- Dirlist[i]
  
  # Make list of rasters per AHN blad
  c_ras <- paste(AHN_tile_full, "/", list.files(AHN_tile_full, pattern = "[c].*.tif"), sep = "")
  #d_ras <- paste(AHN_tile_full, "/", list.files(AHN_tile_full, pattern = "[d].*.tif"), sep = "")
  
  ## Stack Rasters, WATCH NUMBER OF LAYERS!
  c_stack <- stack(c_ras[1],c_ras[2],c_ras[3],c_ras[4],c_ras[5],c_ras[6], c_ras[7], c_ras[8])
  #d_stack <- stack(d_ras[1],d_ras[2],d_ras[3],d_ras[4],d_ras[5],d_ras[6], d_ras[7], d_ras[8])
  writeRaster(c_stack, paste(Loc_new_rasters, "/c_", tile_name, sep=""), "GTiff")
}
  
  ## Stack Rasters, WATCH NUMBER OF LAYERS!
 # assign(paste("c_", tile_name, sep = ""), stack(c_ras[1],c_ras[2],c_ras[3],c_ras[4],c_ras[5],c_ras[6], c_ras[7], c_ras[8]))
#  assign(paste("d_", tile_name, sep = ""), stack(d_ras[1],d_ras[2],d_ras[3],d_ras[4],d_ras[5],d_ras[6], d_ras[7], d_ras[8]))
#  plot(paste("d_", tile_name, sep = ""))
#}



# Stack rasters
  

# Project rasters
  
  
  
  
  
  








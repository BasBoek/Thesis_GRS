# Author: Bastiaen Boekelo
# Date: June 3, 2015
# Description: project and stack point density rasters

rm(list=ls())

library(raster)
library(rgdal)

## Set your folder location where your rasters folders are stored
workspace_files <- "D:/Workspace_LiDAR/Rasters_25m/"
Loc_new_rasters <- "D:/Workspace_LiDAR/Raster_stacked_25m/"

## Get a list of all folders
source("R/List_Directories.R")
Dirlist <- Get_Dirs(workspace_files)
Dirlist <- paste(Dirlist, sep="")
Dirlist_full <- paste(workspace_files, Dirlist, sep="")

## Listing, loading and stacking
RD_new <- crs("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
nr_folders <- length(Dirlist)
for(i in 1:nr_folders){
  AHN_tile_full <- Dirlist_full[i]
  tile_name <- Dirlist[i]
  
  # Make list of rasters per AHN blad
  c_ras <- paste(AHN_tile_full, "/", list.files(AHN_tile_full, pattern = "[c].*.tif"), sep = "")
  
  ## Stack Rasters, WATCH NUMBER OF LAYERS!
  c_stack <- stack(c_ras[1],c_ras[2],c_ras[3],c_ras[4],c_ras[5],c_ras[6], c_ras[7], c_ras[8])
  projection(c_stack) <- RD_new
  
  # Calculate total point counts in voxel
  sum_points <- c_stack[[1]] + c_stack[[2]] + c_stack[[3]] + c_stack[[4]] + c_stack[[5]] + c_stack[[6]] + c_stack[[7]] + c_stack[[8]]
  
  # Setting all pixels with less than 100 point counts to NA
  c_stack[sum_points<100] <- NA
  
  # Normalize the data for the difference in total points
  #c_points_norm <- c_stack/sum_points * 100
  
  # Normalize the data for the difference in height
  c_height_norm <- brick(c_stack[[1]]*6.6667, c_stack[[2]]*3.333, c_stack[[3]]*2, c_stack[[4]], c_stack[[5]]/3, c_stack[[6]]/5, c_stack[[7]]/10, c_stack[[8]]/60)
  
  new_sum <- c_height_norm[[1]] + c_height_norm[[2]] + c_height_norm[[3]] + c_height_norm[[4]] + c_height_norm[[5]] + c_height_norm[[6]] + c_height_norm[[7]] + c_height_norm[[8]]
    
  # Normalization of both
  #c_height_and_points_norm <- c_height_norm/new_sum * 100
  
  ## Write the rasters
  
  #writeRaster(c_stack, paste(Loc_new_rasters, "/c_", tile_name, sep=""), "GTiff", overwrite=TRUE)
  #writeRaster(sum_points, paste(Loc_new_rasters, "/c_point_sum_", tile_name, sep=""), "GTiff", overwrite=TRUE)
  #writeRaster(c_points_norm, paste(Loc_new_rasters, "/c_points_norm_", tile_name, sep=""), "GTiff", overwrite=TRUE)
  #writeRaster(c_height_norm, paste(Loc_new_rasters, "/c_height_norm_", tile_name, sep=""), "GTiff", overwrite=TRUE)
  #writeRaster(c_height_and_points_norm, paste(Loc_new_rasters, "/c_points_heights_norm_", tile_name, sep=""), "GTiff", overwrite=TRUE)
  writeRaster(new_sum, paste(Loc_new_rasters, "/c_new_sum_per_m", tile_name, sep=""), "GTiff", overwrite=TRUE)
  
  
}




# Author: Bastiaen Boekelo
# Date: June 3, 2015
# Description: project and stack point density rasters

rm(list=ls())

library(raster)
library(rgdal)

## Set your folder location where your rasters folders are stored
workspace_files <- "D:/Workspace_LiDAR/Rasters_25m/Without_Houses/"
Loc_new_rasters <- "D:/Workspace_LiDAR/Raster_stacked_25m/Output3/"

## Get a list of all folders
source("R/Create_Raster_Products/List_Directories.R")
Dirlist <- Get_Dirs(workspace_files)
Dirlist <- paste(Dirlist, sep="")
Dirlist_full <- paste(workspace_files, Dirlist, sep="")

## Listing, loading and stacking
RD_new <- crs("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
nr_folders <- length(Dirlist)
for(i in 1:nr_folders){
  AHN_tile_full <- Dirlist_full[i]
  tile_name <- Dirlist[i]
  
  print(tile_name)
  
  # Make list of rasters per AHN blad
  c_ras <- paste(AHN_tile_full, "/", list.files(AHN_tile_full, pattern = "[c].*.tif"), sep = "")
  
  # Only proceed when Lastools made a raster (enough points were present for raster creation)
  if(length(c_ras) > 1){
    ## Stack Rasters, WATCH NUMBER OF LAYERS!
    c_stack <- stack(c_ras[1],c_ras[2],c_ras[3],c_ras[4],c_ras[5],c_ras[6], c_ras[7], c_ras[8])
    projection(c_stack) <- RD_new
  
    # Calculate total point counts in voxel
    sum_points <- c_stack[[1]] + c_stack[[2]] + c_stack[[3]] + c_stack[[4]] + c_stack[[5]] + c_stack[[6]] + c_stack[[7]] + c_stack[[8]]
  
    # Calculating all points till 10 m
    sum_points_till_10m <- c_stack[[1]] + c_stack[[2]] + c_stack[[3]] + c_stack[[4]] + c_stack[[5]]
  
    # Setting all pixels with less than 100 point counts to NA
    c_stack[sum_points_till_10m<100] <- NA
    mask <- sum_points_till_10m
    mask[mask==NA] <- -8888
    mask[mask<100] <- -9999

    # Normalize data for height
    height_norm <- brick(c_stack[[1]]*6.6667, c_stack[[2]]*4, c_stack[[3]]*2, c_stack[[4]]/4, c_stack[[5]]/5, c_stack[[6]]/10, c_stack[[7]]/10, c_stack[[8]]/50)
  
    # Normalize the data for the difference in total points
    points_norm <- c_stack/sum_points * 100
  
    # Normalize the data for the difference in height and points, 
    height_point_skew <- brick(points_norm[[1]]*6.6667, points_norm[[2]]*3.333, points_norm[[3]]*2, points_norm[[4]], points_norm[[5]]/3, points_norm[[6]]/5, points_norm[[7]]/10, points_norm[[8]]/60)
  
    new_sum <- height_point_skew[[1]] + height_point_skew[[2]] + height_point_skew[[3]] + height_point_skew[[4]] + height_point_skew[[5]] + height_point_skew[[6]] + height_point_skew[[7]] + height_point_skew[[8]]
    
    # Normalization of both
    height_points_norm <- height_point_skew/new_sum * 100
  
    ## Write the rasters
    writeRaster(c_stack, paste(Loc_new_rasters, "/c_", tile_name, sep=""), "GTiff", overwrite=TRUE)
    writeRaster(sum_points, paste(Loc_new_rasters, "/c_point_sum_", tile_name, sep=""), "GTiff", overwrite=TRUE)
    writeRaster(mask, paste(Loc_new_rasters, "/mask_", tile_name, sep=""), "GTiff", overwrite=TRUE)
    writeRaster(points_norm, paste(Loc_new_rasters, "/points_norm_", tile_name, sep=""), "GTiff", overwrite=TRUE)
    writeRaster(height_norm, paste(Loc_new_rasters, "/height_norm_", tile_name, sep=""), "GTiff", overwrite=TRUE)
    writeRaster(height_point_skew, paste(Loc_new_rasters, "/height_point_skew_", tile_name, sep=""), "GTiff", overwrite=TRUE)
    writeRaster(height_points_norm, paste(Loc_new_rasters, "/height_points_norm", tile_name, sep=""), "GTiff", overwrite=TRUE)
    writeRaster(new_sum, paste(Loc_new_rasters, "/c_new_sum_per_m", tile_name, sep=""), "GTiff", overwrite=TRUE)
  }
}




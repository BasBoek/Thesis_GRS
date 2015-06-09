# Author: Bastiaen Boekelo
# Date: June 3, 2015
# Description: project and stack point density rasters

rm(list=ls())

library(raster)
library(tiff)

## Set your folder location where your rasters folders are stored
workspace_files <- "D:/Workspace_LiDAR/3_Rasters_25m/"
Loc_new_rasters <- "D:/Workspace_Raster/Stacked_Rasters_Unmerged/"

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
  d_ras <- paste(AHN_tile_full, "/", list.files(AHN_tile_full, pattern = "[d].*.tif"), sep = "")
  nr_ras <- length(c_ras) # For later (writing to TIF)
  
  ## Stack Rasters, WATCH NUMBER OF LAYERS!
  c_stack <- stack(c_ras[1],c_ras[2],c_ras[3],c_ras[4],c_ras[5],c_ras[6], c_ras[7], c_ras[8])
  d_stack <- stack(d_ras[1],d_ras[2],d_ras[3],d_ras[4],d_ras[5],d_ras[6], d_ras[7], d_ras[8])
  projection(c_stack) <- RD_new
  projection(d_stack) <- RD_new
  writeRaster(c_stack, paste(Loc_new_rasters, "/c_", tile_name, sep=""), "GTiff", overwrite=TRUE)
  writeRaster(d_stack, paste(Loc_new_rasters, "/d_", tile_name, sep=""), "GTiff", overwrite=TRUE)
}





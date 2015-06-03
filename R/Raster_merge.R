# Author: Bastiaen Boekelo
# Date: June 3, 2015
# Description: merge, project and stack point density rasters

rm(list=ls()) 
library(raster)
library(tiff)
library(rgdal)
## Set your folder location where your rasters folders are stored
workspace_files <- "F:/Workspace_LiDAR/3_Rasters_25m/"

## Get a list of all folders
source("R/List_Directories.R")
Dirlist <- Get_Dirs(workspace_files)
#Dirlist <- list.dirs(workspace_files)
Dirlist <- paste(workspace_files, Dirlist, sep="")

## Loading rasters

# Listing count and density rasters in directory

One_Dir <- Dirlist[2]
# Make list of rasters per AHN blad
c_ras <- paste(One_Dir, "/", list.files(One_Dir, pattern = "[c].*.tif"), sep = "")
d_ras <- paste(One_Dir, "/", list.files(One_Dir, pattern = "[d].*.tif"), sep = "")

## Stack Rasters, WATCH NUMBER OF LAYERS!
count_stack <- stack(c_ras[1],c_ras[2],c_ras[3],c_ras[4],c_ras[5],c_ras[6], c_ras[7], c_ras[8])
density_stack <- stack(d_ras[1],d_ras[2],d_ras[3],d_ras[4],d_ras[5],d_ras[6], d_ras[7], d_ras[8])


#for(i in 1:nr_ras){
#  layername <- paste("layer", as.character(i), sep="") 
#  file_location <- density_rasters[i]
#  layername <- raster(file_location, as.is = T)
#  plot(layername)
#}




# Stack rasters
  

# Project rasters
  
  
  
  
  
  








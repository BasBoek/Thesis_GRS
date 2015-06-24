# Owner: Bastiaen Boekelo
# Date: June 17, 2015
# Goal: Supervised Classification of the height rasters to obtain vegetation classes

rm(list=ls())

library(rasclass)
library(raster)

#NOTE ###############################
#The following object is masked from ‘package:raster’:
  
#  writeRaster
####################################################

# Loading the training areas and remove redundant data
Training_areas <- shapefile("data/Training_Areas/Training_Areas_AfterRevision2.shp")
Training_areas <- Training_areas[2]

# Loading the raster
source_loc <- "D:/Workspace_Raster/CLASSIFICATION/Testing_Data/"
Den_ras <- stack(paste(source_loc, "height_points_norm39fn2.tif", sep=""))
test <- as.data.frame(Den_ras)

cm01_25 <- Den_ras[[1]]
cm25_100 <- Den_ras[[2]] + Den_ras[[3]]
m1_5 <- Den_ras[[4]]
m5_10 <- Den_ras[[5]]
m10_20 <- Den_ras[[6]]
m20_30 <- Den_ras[[7]]

New_ras <- brick(cm01_25,cm25_100,m1_5,m5_10,m10_20,m20_30)
plot(New_ras)

writeRaster(New_ras, paste(source_loc, "6_bands_39fn2.tif", sep=""), "GTiff", overwrite=TRUE)


## GO TO THIS SITE
http://cran.r-project.org/web/packages/rasclass/rasclass.pdf
classifyRasclass

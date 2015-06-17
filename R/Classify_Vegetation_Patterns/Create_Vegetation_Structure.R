# Owner: Bastiaen Boekelo
# Date: June 17, 2015
# Goal: Supervised Classification of the height rasters to obtain vegetation classes

rm(list=ls())


library(raster)

Training_areas <- shapefile("data/Training_Areas/Training_Areas_AfterRevision.shp")
str(Training_areas)

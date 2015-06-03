library(raster)
library(rgdal)
library(sp)


DownloadCitiesNL <- function(statelevel) {
  nlCity <- getData('GADM',country='NLD', level = statelevel)
}

nederland <- DownloadCitiesNL(1)
Province <- nederland[4,]

# Reproject Province from WGS84 to RD_new
newproj <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
Province <- spTransform(x = Province, CRSobj = newproj)

shapefile(Province, filename = "D:/Documents/BASTIAEN/WUR/laztest/Gelderland.shp", overwrite=T)

plot(Province)

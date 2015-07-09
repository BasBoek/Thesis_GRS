# LISTING THE RASTERS
# Getting All tfw rasters in the source folder, and rename characters as tif

List_KM_Rasters <- function(workspace_source){
  raslist <- list.files(workspace_source, pattern=".tfw", full.names=T)
  ToRemove <- nchar(raslist[1]) - 3
  raslist <<- paste(substring(list.files(workspace_source, pattern="*.tfw", full.names=T),1, ToRemove), "tif", sep="")
}

###################################################
### code chunk number 2: loading_data
###################################################
# load the library
rm(list=ls())

library(biomod2)
library(raster)
library(rgdal)

## WHERE ARE THE PREDICTOR FILES??
setwd("D:/R_Projects/Thesis_GRS2")
foldername_predictors <- "LUVEG"
workspace_training <- paste("D:/SDM/Input_Rasters_KM2/training/", foldername_predictors, sep="")


####################################################
###To get the coordinates of the study area in table
####################################################
predictor.files <- list.files(workspace_training, pattern = ".tif$", full.names=T)  
predictor.files

# load our species data
DataSpecies <- read.csv("data/Bee_data/Bee_data_SDM_input_All_sp_sep_columns.csv", sep="", header=T)

# the name of studied species
myRespName <- "Lasioglossum_sexstrigatum"

# the presence/absences data for our species 
myResp <- as.numeric(DataSpecies[,myRespName])

# the XY coordinates of species data
myRespXY <- DataSpecies[,c("X_ArcGIS","Y_ArcGIS")]

# load the environmental raster layers (could be .img, ArcGIS 
# rasters or any supported format by the raster package)

myExpl = stack(predictor.files)

###################################################
### code chunk number 3: formating_data
###################################################
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     PA.nb.rep = 2,
                                     PA.nb.absences = 1250, # One species has 914 records. There are 2643 unique locations. Choosing 1000 PA is quite a lot (which is good for GLM SDM, and it can vary enough to see if there is many variation in model performance / var_imp etc..). 
                                     PA.strategy = 'random')

# 
# ###################################################
# ### code chunk number 4: print_formating_data
# ###################################################
# myBiomodData
# 
# 
# ###################################################
# ### code chunk number 5: plot_formating_data
# ###################################################
# plot(myBiomodData)
# 
# 

# ###################################################
# ### code chunk number 7: MODELLING
# ###################################################
# # 3. Computing the models 

myBiomodOption <- BIOMOD_ModelingOptions()
setwd("D://SDM/SDM_Output")
myBiomodModelOut <- BIOMOD_Modeling( 
                           myBiomodData, 
                           models = c('GLM'), 
                           models.options = myBiomodOption, # Default is a quadratic model
                           NbRunEval=2, 
                           DataSplit=75, 
                           Prevalence=0.5, # So absences & presences are equally weighted
                           VarImport=10,
                           models.eval.meth = c('ROC', 'TSS', 'ACCURACY'),
                           SaveObj = TRUE,
                           rescal.all.models = TRUE,
                           do.full.models = FALSE,
                           modeling.id = paste("Model_", myRespName, "_BWB", sep=""))

#####################################################################################
### Edit and save models evaluation scores and variables importance on hard drive ###
#####################################################################################
VarImp <- get_variables_importance(myBiomodModelOut)
VarImp <- as.data.frame(VarImp)
VarImp <- as.data.frame(t(VarImp))
VarImp$Species <- myRespName
Rownames <- rownames(VarImp)
VarImp$Run <- as.numeric(substring(Rownames, nchar(Rownames)-4, nchar(Rownames)-4))
VarImp$PA <- as.numeric(substring(Rownames, nchar(Rownames), nchar(Rownames)))

Eval <- get_evaluations(myBiomodModelOut)
Eval <- as.data.frame(Eval)
Eval <- as.data.frame(t(Eval))
Eval$Species <- myRespName
Rownames <- rownames(Eval)
Eval$Statistic <- substring(Rownames, 1, nchar(Rownames)-13)
Eval$Run <- as.numeric(substring(Rownames, nchar(Rownames)-4, nchar(Rownames)-4))
Eval$PA <- as.numeric(substring(Rownames, nchar(Rownames), nchar(Rownames)))

write.table(Eval, paste("D:/SDM/SDM_Output/", myRespName, "_Evaluation.txt", sep=""),sep=" ")
write.table(VarImp, paste("D:/SDM/SDM_Output/", myRespName, "_Variable_Imp.txt", sep=""),sep=" ")

###################################################
### code chunk number 13: projection
###################################################
# projection over the South NL under current conditions

workspace_full_area <- paste("D:/SDM/Input_Rasters_KM2/full_area/", foldername_predictors, sep="")
predictors_full <- list.files(workspace_full_area, pattern='.tif$', full.names=T)
myExpl_completearea <- stack(predictors_full)

myBiomodProj <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut,
                         new.env = myExpl_completearea,
                         proj.name = 'current',
                         selected.models = 'all',
                         binary.meth = c(NULL, 'ROC'),
                         compress = 'xz',
                         clamping.mask = F,
                         output.format = '.tif')

# summary of created oject
myBiomodProj

# files created on hard drive
list.files("Anthcard/proj_current/")



###################################################
### code chunk number 14: projection_curent_plot
###################################################
# make some plots sub-selected by str.grep argument
plot(myBiomodProj, str.grep = 'GLM')


###################################################
### code chunk number 15: projection_curent_getProj
###################################################
# if you want to make custom plots, you can also get the projected map
myCurrentProj <- get_predictions(myBiomodProj)
myCurrentProj



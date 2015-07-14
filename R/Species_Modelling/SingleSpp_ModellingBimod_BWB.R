
###################################################
### code chunk number 2: loading_data
###################################################
# load the library
rm(list=ls())

library(biomod2)
library(raster)
library(rgdal)

## WHERE ARE THE PREDICTOR FILES??

foldername_predictors <- "LUVEG"
workspace_training <- paste("D:/SDM/Input_Rasters_KM2/training/", foldername_predictors, sep="")


####################################################
###To get the coordinates of the study area in table
####################################################
predictor.files <- list.files(workspace_training, pattern = ".tfw", full.names=T)  
predictor.files <- gsub(".tfw", ".tif", predictor.files)  
predictor.files

# load our species data
DataSpecies <- read.csv("data/Bee_data/Bee_data_SDM_input_All_sp_sep_columns.csv", sep="", header=T)

# the name of studied species
myRespName <- 'Bombus_pascuorum'

# the presence/absences data for our species 
myResp <- as.numeric(DataSpecies[,myRespName])

# the XY coordinates of species data
myRespXY <- DataSpecies[,c("X_ArcGIS","Y_ArcGIS")]

# load the environmental raster layers (could be .img, ArcGIS 
# rasters or any supported format by the raster package)

myExpl = stack(predictor.files)
plot(myExpl)
###################################################
### code chunk number 3: formating_data
###################################################
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     PA.nb.rep = 5,
                                     PA.nb.absences = 1250, # One species has 914 records. There are 2643 unique locations. Choosing 1000 PA is quite a lot (which is good for GLM SDM, and it can vary enough to see if there is many variation in model performance / var_imp etc..). 
                                     PA.strategy = 'random')


###################################################
### code chunk number 4: print_formating_data
###################################################
myBiomodData


###################################################
### code chunk number 5: plot_formating_data
###################################################
plot(myBiomodData)


###################################################
### code chunk number 6: modeling_options
###################################################
# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()


###################################################
### code chunk number 7: modeling
###################################################
# 3. Computing the models 

myBiomodModelOut <- BIOMOD_Modeling( 
                           myBiomodData, 
                           models = c('GLM'), 
                           models.options = myBiomodOption, # Default is a quadratic model
                           NbRunEval=5, 
                           DataSplit=80, 
                           Prevalence=0.5, # So absences & presences are equally weighted
                           VarImport=5,
                           models.eval.meth = c('ROC', 'TSS', 'ACCURACY'),
                           SaveObj = TRUE,
                           rescal.all.models = TRUE,
                           do.full.models = FALSE,
                           modeling.id = paste("Model_", myRespName, "_BWB", sep=""))

### save models evaluation scores and variables importance on hard drive
capture.output(get_evaluations(myBiomodModelOut), file=file.path(myRespName, 
      paste("D:/SDM/SDM_Output/", myRespName,"_Model_Eval_BWB.csv", sep="")))

capture.output(get_variables_importance(myBiomodModelOut), file=file.path(myRespName, 
      paste("D:/SDM/SDM_Output/", myRespName,"_Model_Var_Imp_BWB.csv", sep="")))               

# ### modeling_summary
# myBiomodModelOut
# 
# # get all models evaluation                                     
# myBiomodModelEval <- get_evaluations(myBiomodModelOut)
#                                  
# # print the dimnames of this object
# dimnames(myBiomodModelEval)
# 
# # let's print the ROC scores of all selected models
# myBiomodModelEval["ROC","Testing.###################################################data",,,]
# 
# # print variable importances                                    
# get_variables_importance(myBiomodModelOut)


###################################################
### code chunk number 13: projection
###################################################
# projection over the South NL under current conditions
myExpl_completearea<-stack(predictor.files <- list.files("~/Desktop/Nullmodel_Butter/p4_5k/complete_area", pattern='.asc$', full.names=T))

myBiomodProj <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut,
                         new.env = myExpl_completearea,
                         proj.name = 'current',
                         selected.models = 'all',
                         binary.meth = NULL, 'ROC',
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



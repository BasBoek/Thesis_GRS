# Author: Bastiaen Boekelo
# July 2015
# Script for SDM-ing all species based on the environmental variables


###################################################
############## LOADING LIBRARIES   ################
###################################################



# load libraries
rm(list=ls())
setwd("D:/R_Projects/Thesis_GRS2")
library(biomod2)
library(raster)
library(rgdal)
source("R/Species_Modelling/Write_SDM_Rasters.R")

## WHERE ARE THE PREDICTOR FILES??
foldername_predictors <- "LUVEG"
workspace_training <- paste("D:/SDM/Input_Rasters_KM2/training/", foldername_predictors, sep="")

####################################################
######## LOADING PREDICTORS & SPECIES DATA #########
####################################################

# load the environmental raster layers 
predictor.files <- list.files(workspace_training, pattern = ".tif$", full.names=T)  
myExpl = stack(predictor.files)

# load our species data
DataSpecies <- read.csv("data/Bee_data/Bee_data_SDM_input_All_sp_sep_columns.csv", sep="", header=T)

# length(unique(DataSpecies$X_Y_COOR)) # Unique locations

# LOOP LOOP LOOP LOOP LOOP LOOP LOOP LOOP LOOP LOOP LOOP LOOP LOOP LOOP LOOP
setwd(paste("D:/SDM/SDM_Output/", foldername_predictors, sep=""))
All_species <- sort(unique(DataSpecies$FULL_SPECIES))

for(i in 1:length(All_species)){
  
  # the name of studied species
  myRespName <- as.character(All_species[i])
  
  # the presence/absences data for our species 
  myResp <- as.numeric(DataSpecies[,myRespName])
  
  # the XY coordinates of species data
  myRespXY <- DataSpecies[,c("X_ArcGIS","Y_ArcGIS")]
  
  ###################################################
  ### code chunk number 3: formating_data
  ###################################################
  myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                       expl.var = myExpl,
                                       resp.xy = myRespXY,
                                       resp.name = myRespName,
                                       PA.nb.rep = 3,
                                       PA.nb.absences = 1250, # One species has 914 records. There are 2643 unique locations. Choosing 1000 PA is quite a lot (which is good for GLM SDM, and it can vary enough to see if there is many variation in model performance / var_imp etc..). 
                                       PA.strategy = 'random')
  
  # plot(myBiomodData)
  # myBiomodData
  
  # ###################################################
  # ### code chunk number 7: MODELLING
  # ###################################################
  
  myBiomodOption <- BIOMOD_ModelingOptions()
  myBiomodModelOut <- BIOMOD_Modeling( 
    myBiomodData, 
    models = c('GLM'), 
    models.options = myBiomodOption, # Default is a quadratic model
    NbRunEval=10, 
    DataSplit=75, 
    Prevalence=0.5, # So absences & presences are equally weighted
    VarImport=2,
    models.eval.meth = c('TSS', "ROC"), # Here must be at least the ones listed that will be used for the ensemble models as well..
    SaveObj = TRUE,
    rescal.all.models = TRUE,
    do.full.models = FALSE)
  modeling.id = paste("Model_", myRespName, "_BWB", sep="")
  
  # summary of created object
  
  #######################################################
  ########## PROJECT ALL SINGLE SDMs PA/RUN   ###########
  #######################################################
  
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
    build.clamping.mask = F,
    do.stack=T)
  
  ######################################################################
  ####################### Building ensemble-models #####################
  ######################################################################
  
  myBiomodEM <- BIOMOD_EnsembleModeling( 
    modeling.output = myBiomodModelOut,
    chosen.models = 'all',                # So only GLM
    em.by='all',                          # Also only GLM
    eval.metric = c('ROC', 'TSS'),         # For me, (A) to make the binary transformation needed for committee averaging computation. Also: (B) to test (and/or evaluate) your ensemble-models forecasting ability (at this step, each ensemble-model (ensemble will be evaluated according to each evaluation metric)
    # How does binary transformation ROC work? Answer: http://www.sciencedirect.com/science/article/pii/S1146609X07000288
    models.eval.meth = c('ROC','TSS'),
    eval.metric.quality.threshold = c(),
    prob.mean = F,
    prob.cv = F,
    prob.ci = F,  
    prob.ci.alpha = 0.05,
    prob.median = T,                      # Very similar to mean, at least for nomad_albugutta 
    committee.averaging = T,              # Average of binary predictions
    prob.mean.weight = F,
    VarImport = 4) # Tried 10, but variation is very little, 4 should definitely be sufficient
  
  #####################################################################################
  ########### Edit and save models evaluation scores and variables importance #########
  #####################################################################################
  VarImp <- get_variables_importance(myBiomodEM)
  VarImp <- as.data.frame(VarImp)
  VarImp$Species <- myRespName
  
  Eval <- get_evaluations(myBiomodEM)
  Eval <- as.data.frame(Eval)
  Eval <- as.data.frame(t(Eval))
  Eval$Species <- myRespName
  
  write.table(Eval, paste("D:/SDM/SDM_Output/", foldername_predictors, "/_Evaluation/", myRespName, "_Evaluation.txt", sep=""),sep=" ", row.names=T)
  write.table(VarImp, paste("D:/SDM/SDM_Output/", foldername_predictors, "/_Var_imp/", myRespName, "_Variable_Imp.txt", sep=""),sep=" ", row.names=T)
  
  
  ####################################################################
  ###### Make ensemble-models projections on current variable ########
  ####################################################################
  
  myBiomodEF <- BIOMOD_EnsembleForecasting( 
    EM.output = myBiomodEM,
    projection.output = myBiomodProj,
    binary.meth = c('ROC'), # Writing extra Rasters stacks. If ROC is chosen, this will contain multiple rasters (e.g. from mean, median, commitee etc) times the number of evaluation metrices chosen in myBiomodEM. Only ROC will be chosen, since optimizing ROC would be the same as optimizing the TSS (Liu et. al, 2013)
    compress = 'xz',
    clamping.mask = F,
    do.stack=T)

  # Removing unnecessary stuff
  removeTmpFiles()
  
  ### Writing the rasters to a output location
  Write_SDM_Rasters(myRespName, foldername_predictors, paste("D:/SDM/SDM_Output/", foldername_predictors, "/_Rasters_SDM_Models/", sep=""))
}


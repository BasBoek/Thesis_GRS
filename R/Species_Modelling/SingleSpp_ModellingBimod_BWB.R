
###################################################
### code chunk number 2: loading_data
###################################################
# load the library
rm(list=ls())

library(biomod2)

####################################################
###To get the coordinates of the study area in table
####################################################
predictor.files <- read.table("data/Predictors/SDM_VEG_vars.csv", header=T)  

predictor.files
studyarea<-raster(predictor.files[12])
studyarea1<-as.data.frame(studyarea, xy = TRUE)
studyarea1<-na.omit(studyarea1)

# load our species data
DataSpecies <- read.csv("data/Bee_data/Bee_data_SDM_input.csv", sep="", header=T)
head(DataSpecies)
summary(DataSpecies)

# the name of studied species
myRespName <- 'Bombus_pascuorum'

# the presence/absences data for our species 
myResp <- as.numeric(DataSpecies[,myRespName])

# the XY coordinates of species data
myRespXY <- DataSpecies[,c("POINT_X","POINT_Y")]


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
                                     PA.nb.rep = 5,
                                     PA.nb.absences =1000,
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
                           models = c('GLM','RF',"GBM"), 
                           models.options = myBiomodOption, 
                           NbRunEval=1, 
                           DataSplit=80, 
                           Prevalence=0.5, 
                           VarImport=3,
                           models.eval.meth = c('ROC'),
                           SaveObj = TRUE,
                           rescal.all.models = TRUE,
                           do.full.models = FALSE,
                           modeling.id = paste(myRespName,"_P4",sep=""))



###################################################
### code chunk number 8: modeling_summary
###################################################
myBiomodModelOut 


###################################################
### code chunk number 9: modeling_model_evaluation
###################################################
# get all models evaluation                                     
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
                                     
# print the dimnames of this object
dimnames(myBiomodModelEval)
                                     
# let's print the TSS scores of Random Forest
myBiomodModelEval["TSS","Testing.data","RF",,]

# let's print the ROC scores of all selected models
myBiomodModelEval["ROC","Testing.###################################################data",,,]



###################################################
### code chunk number 10: modeling_variable_importance
###################################################
# print variable importances                                    
get_variables_importance(myBiomodModelOut)



### code chunk number 11: ensemble_modeling
###################################################
myBiomodEM <- BIOMOD_EnsembleModeling( 
                     modeling.output = myBiomodModelOut,
                     chosen.models = 'all',
                     em.by='all',
                     eval.metric = c('ROC'),
                     models.eval.meth = c('ROC'),
                     eval.metric.quality.threshold = c(),
                     prob.mean = F,
                     prob.cv = F,
                     prob.ci = F,
                     prob.ci.alpha = 0.05,
                     prob.median = T,
                     committee.averaging = F,
                     prob.mean.weight = T,
                     prob.mean.weight.decay = 'proportional',
                     VarImport = 1)                           ######## What is this???????

get_evaluations(myBiomodEM)

ensemble_models_names <- BIOMOD_LoadModels(myBiomodEM)
ensemble_models_names
vi <- list()
for (mod in ensemble_models_names){
  cat("\n> variables importance of ", mod)
  vi <- c(vi, variables_importance(model=get(mod), data=get_formal_data(myBiomodModelOut,'expl.var'), method="full_rand", nb_rand=2))
}
names(vi)<-ensemble_models_names
vi
vi1<-as.data.frame(vi)
vi1$New_Median_average<-(vi1[1]+vi1[2])/2;vi1$New_Wmean_average<-(vi1[3]+vi1[4])/2



###################################################
### code chunk number 12: ensemble_modeling_outputs
###################################################
# print summary                     
myBiomodEM
                     
# get evaluation scores
get_evaluations(myBiomodEM)

# print variable importances                                    
get_variables_importance(myBiomodModelOut)


###################################################
### code chunk number 13: projection_curent
###################################################
# projection over the globe under current conditions
myExpl_completearea<-stack(predictor.files <- list.files("~/Desktop/Nullmodel_Butter/p4_5k/complete_area", pattern='.asc$', full.names=T))

myBiomodProj <- BIOMOD_Projection(
                         modeling.output = myBiomodModelOut,
                         new.env = myExpl_completearea,
                         proj.name = 'current',
                         selected.models = 'all',
                         binary.meth = 'ROC',
                         compress = 'xz',
                         clamping.mask = F,
                         output.format = '.img')

# summary of crated oject
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


###################################################
### code chunk number 16: projection_future
###################################################
# load environmental variables for the future. 
myExplFuture = stack( system.file( "external/bioclim/future/bio3.grd",
                                 package="biomod2"),
                    system.file( "external/bioclim/future/bio4.grd",
                                 package="biomod2"),
                    system.file( "external/bioclim/future/bio7.grd",
                                 package="biomod2"),
                    system.file( "external/bioclim/future/bio11.grd",
                                 package="biomod2"),
                    system.file( "external/bioclim/future/bio12.grd",
                                 package="biomod2"))

myBiomodProjFuture <- BIOMOD_Projection(
                              modeling.output = myBiomodModelOut,
                              new.env = myExplFuture,
                              proj.name = 'future',
                              selected.models = 'all',
                              binary.meth = 'TSS',
                              compress = 'xz',
                              clamping.mask = T,
                              output.format = '.grd')
                              



###################################################
### code chunk number 17: projection_current_plot
###################################################
# make some plots, sub-selected by str.grep argument
plot(myBiomodProjFuture, str.grep = 'MARS')


###################################################
### code chunk number 18: EnsembleForecasting_current
###################################################
myBiomodEF <- BIOMOD_EnsembleForecasting( 
                      EM.output = myBiomodEM,
                      projection.output = myBiomodProj,
                      binary.meth = 'ROC',
                      compress = 'xz',
                      clamping.mask = F,
                      output.format = '.img')


###################################################
### code chunk number 19: EnsembleForecasting_loading_res
###################################################
myBiomodEF


###################################################
### code chunk number 20: EnsembleForecasting_plotting_res
###################################################
# reduce layer names for plotting convegences
plot(myBiomodEF)

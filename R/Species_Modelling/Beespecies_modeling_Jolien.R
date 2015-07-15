
setwd("C:/Users/Jolien.Morren/Research/Methods/Modeling")
setwd("~/Modeling")
###################################################
### code chunk number 2: LoadSp_1
###################################################
# 1. loading species occurrences data
# 1. loading species occurrences data
library(biomod2)
library(raster)
library(rgdal)

####################################################
###To get the coordinates of the study area in table
####################################################
predictor.files <- list.files("~/Modeling", pattern='.asc$', full.names=T);#predictor.files <- list.files("D:/Jesus/4th_Research_Aug2013/SDM_Butter_biomod/Bees_p4_biomod_SNOWFALL/training", pattern='.asc$', full.names=T)
predictor.files
#studyarea<-raster(predictor.files[12])
#studyarea1<-as.data.frame(studyarea, xy = TRUE)
#studyarea1<-na.omit(studyarea1)

# load our species data
DataSpecies <- read.csv("Beespecies_R_recordsfilter.csv")
head(DataSpecies)
summary(DataSpecies)


###################################################
### code chunk number 3: LoadEnv_1
###################################################
# 2. loading environmental data

myExpl = stack(predictor.files)
plot(myExpl)

a<-na.omit(as.data.frame(myExpl))
b<-cor(a)
write.table(b)

#pairs(myExpl)

###################################################
### code chunk number 4: Loop_1
###################################################
names(DataSpecies)
# define the species of interest

sp.names <- c("Andrena.angustior","Andrena.argentata","Andrena.barbilabris","Andrena.bicolor",
              "Andrena.bimaculata","Andrena.carantonica","Andrena.chrysosceles","Andrena.cineraria",
              "Andrena.dorsata","Andrena.flavipes","Andrena.fucata","Andrena.fulva",
              "Andrena.haemorrhoa","Andrena.helvola","Andrena.labialis","Andrena.labiata",
              "Andrena.minutula","Andrena.nigroaenea","Andrena.nitida","Andrena.ovatula",
              "Andrena.praecox","Andrena.proxima","Andrena.rosae","Andrena.semilaevis",
              "Andrena.subopaca","Andrena.synadelpha","Andrena.tibialis","Andrena.vaga",
              "Andrena.varians","Andrena.ventralis","Andrena.wilkella","Anthophora.furcata",
              "Anthophora.plumipes","Bombus.hortorum","Bombus.hypnorum","Bombus.lapidarius",
              "Bombus.lucorum","Bombus.muscorum","Bombus.pascuorum","Bombus.pratorum","Bombus.ruderarius",
              "Bombus.terrestris","Coelioxys.inermis","Colletes.cunicularius","Halictus.confusus",
              "Halictus.rubicundus","Halictus.tumulorum","Hylaeus.communis","Hylaeus.hyalinatus",
              "Lasioglossum.albipes","Lasioglossum.calceatum","Lasioglossum.fulvicorne","Lasioglossum.laticeps",
              "Lasioglossum.leucopus","Lasioglossum.lucidulum","Lasioglossum.malachurum",
              "Lasioglossum.morio","Lasioglossum.pauxillum","Lasioglossum.semilucens","Lasioglossum.sexnotatum",
              "Lasioglossum.sexstrigatum","Lasioglossum.xanthopus","Lasioglossum.zonulum","Megachile.circumcincta",
              "Megachile.maritima","Nomada.flavoguttata","Nomada.goodeniana","Nomada.lathburiana","Nomada.panzeri",
              "Nomada.signata","Nomada.striata","Osmia.cornuta","Osmia.rufa","Sphecodes.crassus",
              "Sphecodes.geoffrellus","Sphecodes.gibbus","Sphecodes.monilicornis"
) #<- try this in excel



###################################################
### code chunk number 7: Lapply_1
###################################################
MyBiomodSF <- function(sp.n){
  

#for(sp.n in sp.names){
  
  myRespName = sp.n

  cat('\n',myRespName,'modeling...')  
  ### definition of data 
  ## i.e keep only the column of our species
  myResp <- as.numeric(DataSpecies[,myRespName])
  
  myRespCoord = DataSpecies[c('X','Y')]
  
  
  
  ### Initialisation
  myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                       expl.var = myExpl,
                                       resp.xy = myRespCoord,
                                       resp.name = myRespName,
                                       PA.nb.rep = 5,
                                       PA.nb.absences = 10*sum(myResp==1,na.rm=T),
                                       PA.strategy = 'random')  
  #plot(myBiomodData)
  

  ### Options definition
  myBiomodOption <- BIOMOD_ModelingOptions(MAXENT = list( path_to_maxent.jar ="C:/Users"))
  
  ### Modelling 
  myBiomodModelOut <- BIOMOD_Modeling( 
    myBiomodData, 
    models = c('MAXENT','GLM','RF','GBM'), 
    models.options = myBiomodOption, 
    NbRunEval=10, #how many times it's going to run, make in 10
    DataSplit=80, 
    Prevalence=0.5, 
    VarImport=5, #to test how good variables are. takes long, important for me, make it 5
    models.eval.meth = c('ROC','TSS'),
    SaveObj = TRUE,
    rescal.all.models = TRUE,
    do.full.models = FALSE, #has something to do with training with 100%, which we do not do
    modeling.id = paste(myRespName,"_JM",sep=""))
  
  ### save models evaluation scores and variables importance on hard drive
  capture.output(get_evaluations(myBiomodModelOut),
                 file=file.path(myRespName, 
                                paste(myRespName,"_formal_models_evaluation_JM.csv", sep="")))
  
  capture.output(get_variables_importance(myBiomodModelOut),
                 file=file.path(myRespName, 
                                paste(myRespName,"_formal_models_variables_importance_JM.csv", sep="")))               

  
  ### Building ensemble-models
  myBiomodEM <- BIOMOD_EnsembleModeling( 
    modeling.output = myBiomodModelOut,
    chosen.models = 'all',                # So only GLM
    em.by='all',                          # Also only GLM
    eval.metric = c('ROC','TSS'),         # How does binary transformation work?
    models.eval.meth = c('ROC','TSS'),
    eval.metric.quality.threshold = c(),
    prob.mean = T,
    prob.cv = T,
    prob.ci = F, # If TRUE, two ensemble models will be build. 
    prob.ci.alpha = 0.05,
    prob.median = T,
    committee.averaging = T, # = Average of binary predictions
    prob.mean.weight = T,
    prob.mean.weight.decay = 'proportional',
    VarImport = 5) #make it 5
  
  ### save models evaluation scores and variables importance on hard drive ENSEMBLE MODELS
  capture.output(get_evaluations(myBiomodEM),
                 file=file.path(myRespName, 
                                paste(myRespName,"_formal_models_evaluation_ENSEMBLE_JM.csv", sep="")))             
  
  ensemble_models_names <- BIOMOD_LoadModels(myBiomodEM)
  ensemble_models_names
  vi <- list()
  for (mod in ensemble_models_names){
    cat("\n> variables importance of ", mod)
    vi <- c(vi, variables_importance(model=get(mod), data=get_formal_data(myBiomodModelOut,'expl.var'), method="full_rand", nb_rand=5))
  }
  names(vi)<-ensemble_models_names
  vi
 
  vi1<-as.data.frame(vi)
  New_Mean_average_ROC<-(vi1[1]+vi1[2]+vi1[3]+vi1[4]+vi1[5])/5;New_median_average_ROC<-(vi1[6]+vi1[7]+vi1[8]+vi1[9]+vi1[10])/5
  New_ca_average_ROC<-(vi1[11]+vi1[12]+vi1[13]+vi1[14]+vi1[15])/5;New_Wmean_average_ROC<-(vi1[16]+vi1[17]+vi1[18]+vi1[19]+vi1[20])/5
  New_Mean_average_TSS<-(vi1[21]+vi1[22]+vi1[23]+vi1[24]+vi1[25])/5;New_median_average_TSS<-(vi1[26]+vi1[27]+vi1[28]+vi1[29]+vi1[30])/5
  New_ca_average_TSS<-(vi1[31]+vi1[32]+vi1[33]+vi1[34]+vi1[35])/5;New_Wmean_average_TSS<-(vi1[36]+vi1[37]+vi1[38]+vi1[39]+vi1[40])/5

  #New_Mean_average_ROC<-(vi1[1]+vi1[2])/2;New_median_average_ROC<-(vi1[3]+vi1[4])/2
  #New_ca_average_ROC<-(vi1[5]+vi1[6])/2;New_Wmean_average_ROC<-(vi1[7]+vi1[8])/2
  #New_Mean_average_TSS<-(vi1[9]+vi1[10])/2;New_median_average_TSS<-(vi1[11]+vi1[12])/2
  #New_ca_average_TSS<-(vi1[13]+vi1[14])/2;New_Wmean_average_TSS<-(vi1[15]+vi1[16])/2
  
  
  new<-cbind(New_Mean_average_ROC,New_median_average_ROC,New_ca_average_ROC,New_Wmean_average_ROC,New_Mean_average_TSS,
             New_median_average_TSS,New_ca_average_TSS,New_Wmean_average_TSS)
  write.csv(new,file=file.path(myRespName,paste(myRespName,"_formal_models_VariableImportance_ENSEMBLE_JM.csv", sep="")))
  
  #capture.output(vi1,
   #              file=file.path(myRespName, 
    #                            paste(myRespName,"_formal_models_VariableImportance_ENSEMBLE_test.csv", sep="")))               
  
  
  
  ### Make projections on current variable
  predictor.files_Complete <- list.files("~/Modeling", pattern='.asc$', full.names=T)
  complete_area<-stack(predictor.files_Complete)
  myBiomodProj <- BIOMOD_Projection(
    modeling.output = myBiomodModelOut,
    new.env = complete_area,
    proj.name = 'current',
    selected.models = 'all',
    binary.meth = c('ROC','TSS'),
    compress = 'xz',
    clamping.mask = F,
    output.format = '.img',
    do.stack=T)
  
  ### Make ensemble-models projections on current variable
  myBiomodEF <- BIOMOD_EnsembleForecasting( 
    EM.output = myBiomodEM,
    projection.output = myBiomodProj,
    binary.meth = c('ROC','TSS'),
    compress = 'xz',
    clamping.mask = F,
    output.format = '.img',
    do.stack=T)
  removeTmpFiles()
  
}



###################################################
### code chunk number 8: Lapply_2 (for looping)
###################################################
myLapply_SFModelsOut <- lapply( sp.names, MyBiomodSF)


###################################################
### code chunk number 9: SnowFold_1 (eval = FALSE) (for simultaniously)
###################################################
#install.packages('snowfall', dependencies=TRUE)


###################################################
### code chunk number 10: SnowFold_2
###################################################
library(snowfall)


###################################################
### code chunk number 11: SnowFold_3 (eval = FALSE)
###################################################
## 
##Init snowfall
library(snowfall)
sfInit(parallel=TRUE, cpus=4 )  ## we select 2 CPUs. If you have 8 CPUs, put 8. 
## 
## ## Export packages
sfLibrary('biomod2', character.only=TRUE)
sfLibrary('rgdal', character.only=TRUE)
## 
## ## Export variables
## sfExport('myResp')
## sfExport('myExpl')
## sfExport('sp.names')
## 
## # you may also use sfExportAll() to export all your workspace variables
sfExportAll()
##
## ## Do the run
mySFModelsOut <- sfLapply( sp.names, MyBiomodSF)
## 
## ## stop snowfall
## sfStop( nostop=FALSE )
## 

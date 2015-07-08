## Author:  Bastiaen Boekelo
## Date:    25-6-2015
## Content: Adjusted script of Jesus Aguirre Guttierez for creation of landscape variables for my own data.
##Extract Fragmentation Variables ORIGINAL LAYERS separated in small landscapes with the moddelling environment tools.

rm(list=ls())
library(raster)
library(SDMTools)
source("R/Create_Input_Variables/List_KM_Rasters.R")

##########################################
########## FRAGSTATS_1FOREST  ############
##########################################

# SET SOURCE & DESTINATION LOCATION (and listing rasters)
workspace_source <- "D:/SDM/Input_Rasters/ZZSplit_Rasters/_Split_Land_Use/LGN6_Fragstats_1forest/"
workspace_dest <- "D:/SDM/Input_Rasters/ZZSplit_Rasters/Split_Tables/Fragstats_1forest/"
List_KM_Rasters(workspace_source)

# Set Name of final table
table_name <- "_Fragstats_1forest"

# DETERMINE NA VALUES!
NA_val <- 9999

for(i in 1:length(raslist)){
  ras<-raster(raslist[i])
  b<-ClassStat(ras,25) # 25 = cell size
  b$NumClasses_J <-sum(as.numeric(b[,1]!=NA_val))#CALCULATE NUMBER OF CLASSES TAKING NODATA AWAY
  b$ED_total_J <-sum(as.numeric(b[b$class!=NA_val,7]))#SUM ED (SUM ED OF DIFFERENT CLASSES)
  b$Area_total_Km2<-sum(as.numeric(b[b$class!=NA_val,3]))/1000000 #Km2
  filename<-as.character(names(ras))
  b$landscape<-filename		#CAPTURE THE LANDSCAPE NUMBER I THE ROW
  b<-b[1,c(39:43)]	#EXTRACT ONLY IMPORTANT COLUMNS
  write.table(b,paste(workspace_dest, filename, ".txt", sep=""), row.names = F)
  print(paste("img",i,"of", length(raslist)))
}
print("done!")

tablist <- list.files(workspace_dest, pattern ="*.txt", full.names=TRUE)
All_records <- do.call("rbind", lapply(tablist, read.table, header = TRUE))
write.csv(All_records, paste(workspace_dest, table_name,".csv", sep=""))







##########################################
########### BEE_SUITABILITY  #############
##########################################

# SET SOURCE LOCATION (and listing rasters)
workspace_source <- "D:/SDM/Input_Rasters/ZZSplit_Rasters/_Split_Land_Use/LGN6_Fragstats_1forest/"
List_KM_Rasters(workspace_source)


# DETERMINE NA VALUES!
NA_val <- NA

for(i in 1:length(raslist)){
  ras<-raster(raslist[i])
  b<-ClassStat(ras,25) # 25 = cell size
  b$NumClasses_J <-sum(as.numeric(b[,1]!=NA_val))#CALCULATE NUMBER OF CLASSES TAKING NODATA AWAY
  b$ED_total_J <-sum(as.numeric(b[b$class!=NA_val,7]))#SUM ED (SUM ED OF DIFFERENT CLASSES)
  b$Area_total_Km2<-sum(as.numeric(b[b$class!=NA_val,3]))/1000000#Km2
  b$Mean_Patch_areaKm2<-sum(as.numeric(b[b$class!=NA_val,10]))/1000000#km2
  filename<-as.character(names(ras))
  b$landscape<-filename  	#CAPTURE THE LANDSCAPE NUMBER I THE ROW
  b<-b[1,c(39:43)]	#EXTRACT ONLY IMPORTANT COLUMNS
  write.table(b,paste(filename,".txt"))
  print(paste("img",i,"on", length(raslist)))
}
print("done!")










#############################################################################################

#PUT ALL LANDSCAPES (TABLES) TOGETHER
tablist<-Sys.glob("*.txt")
full_LGN6<-do.call("rbind", lapply(tablist, read.table, header = TRUE))
write.csv(full_LGN6,"Full_frag_table_LGN6.csv")





#MERGE all Tables
setwd("D:/Jesus/2nd_RESEARCH_Aug2012/New_Analysis_Sep3_14/3_Tables_with_results")
list.files()
t1<-read.csv("Fragstats_1900RESULTS_ProxMean.csv")#PROXIMITY INDEX MEAN (higher=More proximal)
t2<-read.csv("Fragstats_1960RESULTS_ProxMean.csv")
t3<-read.csv("Fragstats_1980RESULTS_ProxMean.csv") 
t4<-read.csv("Fragstats_LGN6RESULTS_ProxMean.csv")
t5<-read.csv("Full_frag_table_1900.csv")#Other Fragmentation metrics
t6<-read.csv("Full_frag_table_1960.csv")
t7<-read.csv("Full_frag_table_1980.csv")
t8<-read.csv("Full_frag_table_LGN6.csv")
t9<-read.csv("Full_ManNat_table_1900.csv")# ED Man Nat metric
t10<-read.csv("Full_ManNat_table_1960.csv")
t11<-read.csv("Full_ManNat_table_1980.csv")
t12<-read.csv("Full_ManNat_table_LGN6.csv")
t13<-read.csv("PSH_results_Ch_lnRatio.csv")# PSH metric

m1=merge(t1,t2,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m2=merge(m1,t3,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m3=merge(m2,t4,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m4=merge(m3,t5,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m5=merge(m4,t6,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m6=merge(m5,t7,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m7=merge(m6,t8,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m8=merge(m7,t9,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m9=merge(m8,t10,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m10=merge(m9,t11,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m11=merge(m10,t12,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)
m12=merge(m11,t13,by.x="CellID_JA",by.y="CellID_JA",all.x=T,all.y=T)

m13<-m12[,c(1:11,14:15,18:19,22:23,26,28,30,32:39)]#select columns of interest; only include PA_AM_Nat, no the others Areas

write.csv(m13,"2_CompFrag_complete.csv")


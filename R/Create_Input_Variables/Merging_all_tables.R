## July 9th
## Author: Bastiaen
## Adjusted script Jesus, Merging input variables

#MERGE all Tables

rm(list=ls())

workspace <- "D:/SDM/Input_Rasters/ZZSplit_Rasters/Split_Tables/"
Filelist <- paste(workspace, list.files(workspace,pattern = ".csv"), sep="")
Filelist
t1<-read.csv(Filelist[1])[,2:4]
t2<-read.csv(Filelist[2])[,2:5]
t3<-read.csv(Filelist[3])[,2:4]
t4<-read.csv(Filelist[4])[,2:5]
t5<-read.csv(Filelist[5])[,2:4]
t6<-read.csv(Filelist[6])[,2:16]
t7<-read.csv(Filelist[7])[,2:17]


m1=merge(t1,t2,by.x="landscape",by.y="landscape",all.x=T,all.y=T)
m2=merge(m1,t3,by.x="landscape",by.y="landscape",all.x=T,all.y=T)
m3=merge(m2,t4,by.x="landscape",by.y="landscape",all.x=T,all.y=T)
m4=merge(m3,t5,by.x="landscape",by.y="landscape",all.x=T,all.y=T)
m5=merge(m4,t6,by.x="landscape",by.y="landscape",all.x=T,all.y=T)
final=merge(m5,t7,by.x="landscape",by.y="landscape",all.x=T,all.y=T)
final$CELL_ID <- 1
for(i in 1:nrow(final)){
  final$CELL_ID[i] <- as.numeric(substring(final$landscape[i], 4, nchar(as.character(final$landscape[i]))))
}
# Making column for point coordinates ArcGIS
final$X_ArcGIS <- 1
final$Y_ArcGIS <- 1
for(i in 1:nrow(final)){
  final$X_ArcGIS[i] <- floor(final$CELL_ID[i] / 1000)*1000+500
  final$Y_ArcGIS[i] <- ((final$CELL_ID[i]/1000 - floor(final$CELL_ID[i]/1000)) *1000)*1000+500
}

# Subsetting VEG variables by area of NoData, writing new table to .csv
VEG_vars <- final[c(43:45,7,14,15,18:25,16)]
VEG_vars <- subset(VEG_vars, VEG_vars[13] > 0.4) # Select areas where NA is less than 60%
VEG_vars <- VEG_vars[1:length(VEG_vars)-1]
write.table(VEG_vars,"data/Predictors/SDM_VEG_vars.csv", row.names = F)

# Idem for LU variables...
LU_vars <- final[c(43:45,5,6,9:10,12,11)]
LU_vars <- subset(LU_vars, LU_vars[9] > 0.4) # Select areas where NA is less than 60%
LU_vars <- LU_vars[1:length(LU_vars)-1]
write.table(LU_vars,"data/Predictors/SDM_LU_vars.csv", row.names = F)

names(final)
# Idem for LUVEG variables...
LUVEG_vars <- final[c(43:45,2,5,12, 28,29,32:42,30)]
LUVEG_vars <- subset(LUVEG_vars, LUVEG_vars[20] > 0.4) # Select areas where NA is less than 60%
LUVEG_vars <- LUVEG_vars[1:length(LUVEG_vars)-1]
write.table(LUVEG_vars,"data/Predictors/SDM_LUVEG_vars.csv", row.names = F)




DataSpecies <- read.csv("data/Bee_data/Bee_data_SDM_input.csv", sep="", header=T)

Test <- merge(VEG_vars,DataSpecies, by.x="CELL_ID",by.y="X_Y_COOR",all.x=T,all.y=F)


LU_vars <- 1,5,8,9,11, 27,28, 29:41

LUVEG_vars <- 2, 3, 5
names(final)
final$CELL_ID[1]
Total_Areas <- final[,c(1,3,4,7,10,12,15,29)]



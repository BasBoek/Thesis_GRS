## July 9th
## Author: Bastiaen
## Adjusted script Jesus, Merging input variables

#MERGE all Tables

rm(list=ls())

setwd("D:/SDM/Input_Rasters/ZZSplit_Rasters/Split_Tables")
Filelist <- list.files(pattern = ".csv")

t1<-read.csv(Filelist[1])[,2:4]
t2<-read.csv(Filelist[2])[,2:4]
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

final[,c(1)]#select columns of interest; only include PA_AM_Nat, no the others Areas
VEG_vars <- final[c(1,6,13,14,16:24,15)]
VEG_vars <- subset(VEG_vars, VEG_vars[14] > 0.4) # Select areas where NA is less than 60%


nrow(VEG_vars)
LU_vars <- 1,5,8,9,11, 27,28, 29:41

LUVEG_vars <- 2, 3, 5
names(final)

Total_Areas <- final[,c(1,3,4,7,10,12,15,29)]


write.table(VEG_vars,"SDM_VEG_vars.csv", row.names = F)

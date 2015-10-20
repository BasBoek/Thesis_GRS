# Author: Bastiaen Boekelo
# September 10, 2015
# Goal: Create three tables of variable importances by model

# Author: Bastiaen Boekelo
# September 2015
# Goal: Getting an overview of the SDM results

rm(list=ls())


ExVar <- "LU"
Vars <- c("LU", "VEG", "LUVEG")

for(ExVar in Vars){
  
  # list the Variable Importance files
  VIPlist <- list.files(paste("D:/SDM/SDM_Output/", ExVar, "/_Var_imp", sep=""), full.names=T)
  
  # Create new table structure
  rownames <- row.names(as.data.frame(read.table(VIPlist[1])))
  Final_VI_list <- as.data.frame(matrix(0,1,length(rownames)))
  colnames(Final_VI_list) <- rownames
  Final_VI_list$Species <- NA
  Final_VI_list <- transform(Final_VI_list, Species = as.character(Species))
  
  for(VIfile in 1:length(VIPlist)){
    Sp_data <- as.data.frame(read.table(VIPlist[VIfile]))
    Final_VI_list[VIfile,(length(rownames)+1)] <- as.character(Sp_data$Species[1])
    for(row in 1:nrow(Sp_data)){
      SumImp=0
      for(column in 1:4){
        newImp <- Sp_data[row,column]
        SumImp <- SumImp + newImp
      }
      MeanImp <- SumImp/4
      Final_VI_list[VIfile,row] <- MeanImp
    }
  }
  write.csv(Final_VI_list, paste("D:/SDM/SDM_SecondaryProducts/", ExVar, "_Var_imp.csv", sep=""), row.names=F)
}
boxplot(Final_VI_list[2:18], main="Variable Importance\nLand Use & Vegetation", col="green", vertical=T, las=1, cex.lab=1.2, xlab="Environmental variable", ylab="Variable Importance")

boxnames <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)

ggplot(Final_VI_list[,2])

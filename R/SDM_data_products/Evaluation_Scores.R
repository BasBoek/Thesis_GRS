# Author: Bastiaen Boekelo
# September 2015
# Goal: Getting an overview of the SDM results

rm(list=ls())

Scores <- data.frame(Species=character(), AUC=numeric(), TSS=numeric(), EXVARS=character(), stringsAsFactors=FALSE)

Vars <- c("LU", "VEG", "LUVEG")

ExVar <- "LU"
Create_table <- function(ExVar){
  for(i in 1:length(Scores_list)){
    Scores_list <- list.files(paste("D:/SDM/SDM_Output/", ExVar, "/_Evaluation", sep=""), full.names=T)
    Sp_data <- as.data.frame(read.table(Scores_list[i]))
    Scores[i,1] <- as.character(Sp_data[1,3])
    Scores[i,2] <- Sp_data[1,1]
    Scores[i,3] <- Sp_data[1,2]
    Scores[i,4] <- ExVar
    names(Scores) <- c("Species", "AUC", "TSS", "EXVARS")
  }
  write.csv(Scores, paste("D:/SDM/SDM_SecondaryProducts/", ExVar, "_Evaluation.csv", sep=""), row.names=F)
}
Create_table("LU")
Create_table("LUVEG")
Create_table("VEG")


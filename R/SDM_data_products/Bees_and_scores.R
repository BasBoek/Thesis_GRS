Bees <- read.table("D:/SDM/Bee_data.txt", sep=" ", header=T)

LUVEG_scores <- read.table("D:/SDM/SDM_SecondaryProducts/LUVEG_Evaluation.csv", header=T, sep=",")
VEG_scores <- read.table("D:/SDM/SDM_SecondaryProducts/VEG_Evaluation.csv", header=T, sep=",")
LU_scores <- read.table("D:/SDM/SDM_SecondaryProducts/LU_Evaluation.csv", header=T, sep=",")



LUVEG <- merge(LUVEG_scores,Bees,by.x="Species",by.y="Species",all.x=T,all.y=T)
VEG <- merge(LUVEG_scores,Bees,by.x="Species",by.y="Species",all.x=T,all.y=T)
LU <- merge(LUVEG_scores,Bees,by.x="Species",by.y="Species",all.x=T,all.y=T)


plot(LU$AUC, VEG$AUC)
plot(VEG$Counts, VEG$AUC)
plot(LU$Counts, LU$AUC)


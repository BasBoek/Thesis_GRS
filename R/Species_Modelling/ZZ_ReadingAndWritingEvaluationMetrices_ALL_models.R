#####################################################################################
### Edit and save models evaluation scores and variables importance on hard drive ###
#####################################################################################
VarImp <- get_variables_importance(myBiomodEM)
VarImp <- as.data.frame(VarImp)
VarImp <- as.data.frame(t(VarImp))
VarImp$Species <- myRespName
Rownames <- rownames(VarImp)
VarImp$Run <- as.numeric(substring(Rownames, nchar(Rownames)-4, nchar(Rownames)-4))
VarImp$PA <- as.numeric(substring(Rownames, nchar(Rownames), nchar(Rownames)))

Eval <- get_evaluations(myBiomodEM)
Eval <- as.data.frame(Eval)
Eval <- as.data.frame(t(Eval))
Eval$Species <- myRespName
Rownames <- rownames(Eval)
Eval$Statistic <- substring(Rownames, 1, nchar(Rownames)-13)
Eval$Run <- as.numeric(substring(Rownames, nchar(Rownames)-4, nchar(Rownames)-4))
Eval$PA <- as.numeric(substring(Rownames, nchar(Rownames), nchar(Rownames)))

write.table(Eval, paste("D:/SDM/SDM_Output/_Evaluation/", myRespName, "_Evaluation.txt", sep=""),sep=" ", row.names=F)
write.table(VarImp, paste("D:/SDM/SDM_Output/_Var_imp/", myRespName, "_Variable_Imp.txt", sep=""),sep=" ", row.names=F)

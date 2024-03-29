# Author: Bastiaen Boekelo
# October 20, 2015
# Goal: gain regression coefficients of two predictions (one with shuffled variables) of VEG

#######################################
########### vEG ANALYSES ############
#######################################

rm(list=ls())
library(raster)

WBdiv_VEG <- raster("D:/SDM/SDM_SecondaryProducts/Diversity_ens_VEG.tif")
pred_VEG <- stack(list.files("D:/SDM/Input_Rasters_KM2/full_area/VEG", full.names=T, pattern=".tif$"))
pred_VEG <- pred_VEG[[2:12]]
ALL <- stack(pred_VEG, WBdiv_VEG)
ALL_df <- as.data.frame(ALL)
ALL_df <- na.omit(ALL_df)
names(ALL_df)

# FILL IN WHICH VARIABLES WILL BE USED - MUST BE THE SAME AS LM !!!
names_vars <- c("EdgeDensity", "Food_Availability", "Mean_Patch_areaKm2_VEG_All", "VEG_All_1", "VEG_All_2", "VEG_All_3", "VEG_All_4", "VEG_All_5", "VEG_All_6", "VEG_All_7", "VEG_All_8")

# Multilinear Regression (after removel ED_total_man_nat)
fit_true <- lm(Diversity_ens_VEG ~ EdgeDensity + Food_Availability + Mean_Patch_areaKm2_VEG_All + VEG_All_1 + VEG_All_2 + VEG_All_3 + VEG_All_4 + VEG_All_5 + VEG_All_6 + VEG_All_7 + VEG_All_8, data=ALL_df)

# Getting parameter coefficients TRUE LM
coefficients <- as.data.frame(coef(fit_true))

# Prediction based on coefficients
new_pred_VEG <- pred_VEG$EdgeDensity*0 + coefficients[1,] + pred_VEG$EdgeDensity * coefficients[2,] + pred_VEG$Food_Availability * coefficients[3,] + pred_VEG$Mean_Patch_areaKm2_VEG_All * coefficients[4,] + pred_VEG$VEG_All_1 * coefficients[5,] + pred_VEG$VEG_All_2 * coefficients[6,] + pred_VEG$VEG_All_3 * coefficients[7,] + pred_VEG$VEG_All_4 * coefficients[8,] + pred_VEG$VEG_All_5 * coefficients[9,] + pred_VEG$VEG_All_6 * coefficients[10,] + pred_VEG$VEG_All_7 * coefficients[11,]
plot(new_pred_VEG)

new_pred_VEG_df <- as.data.frame(new_pred_VEG)


num_rep <- 5
R2_table <- as.data.frame(matrix(NA,num_rep,length(names_vars)))
colnames(R2_table) <- names_vars
R2_table
for(ii in 1:length(names_vars)){
  for(i in 1:num_rep){
    ALL_df_copy <- ALL_df
    Var_selected <- ALL_df_copy[,names_vars[ii]]
    Var_shuffled <- sample(Var_selected)
    ALL_df_copy[,names_vars[ii]] <- Var_shuffled
    
    # DATA COMES FROM ALL_df_copy !!
    fit_FALSE <- lm(Diversity_ens_VEG ~ EdgeDensity + Food_Availability + Mean_Patch_areaKm2_VEG_All + VEG_All_1 + VEG_All_2 + VEG_All_3 + VEG_All_4 + VEG_All_5 + VEG_All_6 + VEG_All_7 + VEG_All_8, data=ALL_df_copy)
    
    # Getting parameter coefficients TRUE LM
    coefficients <- as.data.frame(coef(fit_FALSE))
    
    new_pred_VEG_False <- pred_VEG$EdgeDensity*0 + coefficients[1,] + pred_VEG$EdgeDensity * coefficients[2,] + pred_VEG$Food_Availability * coefficients[3,] + pred_VEG$Mean_Patch_areaKm2_VEG_All * coefficients[4,] + pred_VEG$VEG_All_1 * coefficients[5,] + pred_VEG$VEG_All_2 * coefficients[6,] + pred_VEG$VEG_All_3 * coefficients[7,] + pred_VEG$VEG_All_4 * coefficients[8,] + pred_VEG$VEG_All_5 * coefficients[9,] + pred_VEG$VEG_All_6 * coefficients[10,] + pred_VEG$VEG_All_7 * coefficients[11,]
    plot(new_pred_VEG)
    
    new_pred_VEG_False_df <- as.data.frame(new_pred_VEG_False)
    predictions <- as.data.frame(cbind(new_pred_VEG_False_df, new_pred_VEG_df))
    colnames(predictions) <- c("new_pred_VEG_False_df", "new_pred_VEG_df")
    new_fit_LMs <- lm(new_pred_VEG_False_df ~ new_pred_VEG_df, data=predictions)
    
    final_r2 <- summary(new_fit_LMs)$r.squared
    
    # filling it into r2 table
    R2_table[i,ii] <- 1-final_r2
    
  }
}

# CHANGE NAME TO SOMETHING GOOD !!!!
write.csv(R2_table, "D:/SDM/SDM_SecondaryProducts/Var_Imp_Diversity/R2_VEGshuffle-VEGreal.csv", row.names=F)








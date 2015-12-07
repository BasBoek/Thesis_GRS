# Author: Bastiaen Boekelo
# October 20, 2015
# Goal: gain regression coefficients of two predictions (one with shuffled variables) of LUVEG

#######################################
########### LUVEG ANALYSES ############
#######################################

rm(list=ls())
library(raster)

# List rasters
WBdiv <- raster("D:/SDM/SDM_SecondaryProducts/Diversity_ens_LUVEG.tif")
pred <- stack(list.files("D:/SDM/Input_Rasters_KM2/full_area/LUVEG", full.names=T, pattern=".tif$"))

pred <- pred[[2:18]]
ALL <- stack(pred, WBdiv)
ALL_df <- as.data.frame(ALL)
ALL_df <- na.omit(ALL_df)

# FILL IN WHICH VARIABLES WILL BE USED - MUST BE THE SAME AS LM !!!
names_vars <- c("LUVEG_1", "LUVEG_2", "LUVEG_20", "LUVEG_3", "LUVEG_30", "LUVEG_4", "LUVEG_5", "LUVEG_6", "LUVEG_60", "LUVEG_7", "LUVEG_8", "Mean_Patch_areaKm2_Veg_LU", "SandySoils_withoutAbroad")

# Multilinear Regression (after removel ED_total_man_nat)
fit_true <- lm(Diversity_ens_LUVEG ~ LUVEG_1 + LUVEG_2 + LUVEG_20 + LUVEG_3 + LUVEG_30 + LUVEG_4 + LUVEG_5 + LUVEG_6 + LUVEG_60 + LUVEG_7 + LUVEG_8 + Mean_Patch_areaKm2_Veg_LU + SandySoils_withoutAbroad, data=ALL_df)


# Getting parameter coefficients
coefficients <- as.data.frame(coef(fit_true))

r2 <- summary(fit_true)$r.squared


# Predicting new raster based on value
new_pred_LUVEG <- pred$LUVEG_1*0 + coefficients[1,] + pred$LUVEG_1 * coefficients[2,] + pred$LUVEG_2 * coefficients[3,] + pred$LUVEG_20 * coefficients[4,] + pred$LUVEG_3 * coefficients[5,] + pred$LUVEG_30 * coefficients[6,] + pred$LUVEG_4 * coefficients[7,] + pred$LUVEG_5 * coefficients[8,] + pred$LUVEG_6 * coefficients[9,] + pred$LUVEG_60 * coefficients[10,] + pred$LUVEG_7 * coefficients[11,] + pred$LUVEG_8 * coefficients[12,] + pred$Mean_Patch_areaKm2_Veg_LU * coefficients[13,] + pred$SandySoils_withoutAbroad * coefficients[14,]


new_pred_LUVEG_df <- as.data.frame(new_pred_LUVEG)


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
    fit_FALSE <- lm(Diversity_ens_LUVEG ~ LUVEG_1 + LUVEG_2 + LUVEG_20 + LUVEG_3 + LUVEG_30 + LUVEG_4 + LUVEG_5 + LUVEG_6 + LUVEG_60 + LUVEG_7 + LUVEG_8 + Mean_Patch_areaKm2_Veg_LU + SandySoils_withoutAbroad, data=ALL_df_copy)
    
    
    # Getting parameter coefficients TRUE LM
    coefficients <- as.data.frame(coef(fit_FALSE))
    
    new_pred_LUVEG_False <- pred$LUVEG_1*0 + coefficients[1,] + pred$LUVEG_1 * coefficients[2,] + pred$LUVEG_2 * coefficients[3,] + pred$LUVEG_20 * coefficients[4,] + pred$LUVEG_3 * coefficients[5,] + pred$LUVEG_30 * coefficients[6,] + pred$LUVEG_4 * coefficients[7,] + pred$LUVEG_5 * coefficients[8,] + pred$LUVEG_6 * coefficients[9,] + pred$LUVEG_60 * coefficients[10,] + pred$LUVEG_7 * coefficients[11,] + pred$LUVEG_8 * coefficients[12,] + pred$Mean_Patch_areaKm2_Veg_LU * coefficients[13,] + pred$SandySoils_withoutAbroad * coefficients[14,]

    
    new_pred_LUVEG_False_df <- as.data.frame(new_pred_LUVEG_False)
    predictions <- as.data.frame(cbind(new_pred_LUVEG_False_df, new_pred_LUVEG_df))
    colnames(predictions) <- c("new_pred_LUVEG_False_df", "new_pred_LUVEG_df")
    
    new_fit_LMs <- lm(new_pred_LUVEG_False_df ~ new_pred_LUVEG_df, data=predictions)
    
    final_r2 <- summary(new_fit_LMs)$r.squared
    
    # filling it into r2 table
    R2_table[i,ii] <- 1-final_r2
    
  }
}

# CHANGE NAME TO SOMETHING GOOD !!!!
write.csv(R2_table, "D:/SDM/SDM_SecondaryProducts/Var_Imp_Diversity/R2_LUVEGshuffle-LUVEGreal.csv", row.names=F)






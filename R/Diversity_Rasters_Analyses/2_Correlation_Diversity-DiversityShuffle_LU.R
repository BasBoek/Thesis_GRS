# Author: Bastiaen Boekelo
# October 20, 2015
# Goal: gain regression coefficients of two predictions (one with shuffled variables) of LU

#######################################
########### LU ANALYSES ############
#######################################

rm(list=ls())
library(raster)

WBdiv_LU <- raster("D:/SDM/SDM_SecondaryProducts/Diversity_ens_LU.tif")
pred <- stack(list.files("D:/SDM/Input_Rasters_KM2/full_area/LU", full.names=T, pattern=".tif$"))
pred <- pred[[2:8]]
ALL <- stack(pred, WBdiv_LU)
ALL_df <- as.data.frame(ALL)
ALL_df <- na.omit(ALL_df)
names(ALL_df)

# FILL IN WHICH VARIABLES WILL BE USED - MUST BE THE SAME AS LM !!!
names_vars <- c("ED_total_ManNat", "Food_Availability", "Mean_Patch_areaKm2_BS", "NumClasses_Fragstat", "SandySoils_withoutAbroad", "Suitability")

# Multilinear Regression (after removel ED_total_man_nat)
fit_true <- lm(Diversity_ens_LU ~ ED_total_ManNat + Food_Availability + Mean_Patch_areaKm2_BS + NumClasses_Fragstat + SandySoils_withoutAbroad + Suitability, data=ALL_df)


# Getting parameter coefficients
coefficients <- as.data.frame(coef(fit_true))

r2 <- summary(fit_true)$r.squared


# Predicting new raster based on value
new_pred_LU <- pred$ED_total_ManNat*0 + coefficients[1,] + pred$ED_total_ManNat * coefficients[2,] + pred$Food_Availability * coefficients[3,] + pred$Mean_Patch_areaKm2_BS * coefficients[4,] + pred$NumClasses_Fragstat * coefficients[5,] + pred$SandySoils_withoutAbroad * coefficients[6,] + pred$Suitability * coefficients[7,]

new_pred_LU_df <- as.data.frame(new_pred_LU)


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
    fit_FALSE <- lm(Diversity_ens_LU ~ ED_total_ManNat + Food_Availability + Mean_Patch_areaKm2_BS + NumClasses_Fragstat + SandySoils_withoutAbroad + Suitability, data=ALL_df_copy)
    
    # Getting parameter coefficients TRUE LM
    coefficients <- as.data.frame(coef(fit_FALSE))
    
    new_pred_LU_False <- pred$ED_total_ManNat*0 + coefficients[1,] + pred$ED_total_ManNat * coefficients[2,] + pred$Food_Availability * coefficients[3,] + pred$Mean_Patch_areaKm2_BS * coefficients[4,] + pred$NumClasses_Fragstat * coefficients[5,] + pred$SandySoils_withoutAbroad * coefficients[6,] + pred$Suitability * coefficients[7,]
    
    new_pred_LU_False_df <- as.data.frame(new_pred_LU_False)
    predictions <- as.data.frame(cbind(new_pred_LU_False_df, new_pred_LU_df))
    colnames(predictions) <- c("new_pred_LU_False_df", "new_pred_LU_df")
    
    new_fit_LMs <- lm(new_pred_LU_False_df ~ new_pred_LU_df, data=predictions)
    
    final_r2 <- summary(new_fit_LMs)$r.squared
    
    # filling it into r2 table
    R2_table[i,ii] <- 1-final_r2
    
  }
}

# CHANGE NAME TO SOMETHING GOOD !!!!
write.csv(R2_table, "D:/SDM/SDM_SecondaryProducts/Var_Imp_Diversity/R2_LUshuffle-LUreal.csv", row.names=F)







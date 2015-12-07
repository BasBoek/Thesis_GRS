# Author: Bastiaen Boekelo
# October 20, 2015
# Goal: gain regression coefficients of the non-collinear predictor variables of LUVEG

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

num_rep <- 10
R2_table <- as.data.frame(matrix(NA,num_rep,length(names_vars)))
colnames(R2_table) <- names_vars
R2_table
for(ii in 1:length(names_vars)){
  for(i in 1:num_rep){
    ALL_df_copy <- ALL_df
    Var_selected <- ALL_df_copy[,names_vars[ii]]
    Var_shuffled <- sample(Var_selected)
    ALL_df_copy[,names_vars[ii]] <- Var_shuffled
    
    fit <- lm(Diversity_ens_LUVEG ~ LUVEG_1 + LUVEG_2 + LUVEG_20 + LUVEG_3 + LUVEG_30 + LUVEG_4 + LUVEG_5 + LUVEG_6 + LUVEG_60 + LUVEG_7 + LUVEG_8 + Mean_Patch_areaKm2_Veg_LU + SandySoils_withoutAbroad, data=ALL_df_copy)
    
    # Getting r2
    r2 <- summary(fit)$r.squared
    
    # filling it into r2 table
    R2_table[i,ii] <- r2
    
  }
}

# CHANGE NAME TO SOMETHING GOOD !!!!
write.csv(R2_table, "D:/SDM/SDM_SecondaryProducts/Var_Imp_Diversity/R2_LUVEG.csv", row.names=F)

# Multilinear Regression (after removel 2 edge densities, food availability and )
fit <- lm(Diversity_ens_LUVEG ~ LUVEG_1 + LUVEG_2 + LUVEG_20 + LUVEG_3 + LUVEG_30 + LUVEG_4 + LUVEG_5 + LUVEG_6 + LUVEG_60 + LUVEG_7 + LUVEG_8 + Mean_Patch_areaKm2_Veg_LU + SandySoils_withoutAbroad, data=ALL_df)

# Getting parameter coefficients
coefficients <- as.data.frame(coef(fit))
coefficients
r2 <- summary(fit)$r.squared
r2



# Predicting new raster based on value
new_pred <- pred$LUVEG_1*0 + coefficients[1,] + pred$LUVEG_1 * coefficients[2,] + pred$LUVEG_2 * coefficients[3,] + pred$LUVEG_20 * coefficients[4,] + pred$LUVEG_3 * coefficients[5,] + pred$LUVEG_30 * coefficients[6,] + pred$LUVEG_4 * coefficients[7,] + pred$LUVEG_5 * coefficients[8,] + pred$LUVEG_6 * coefficients[9,] + pred$LUVEG_60 * coefficients[10,] + pred$LUVEG_7 * coefficients[11,] + pred$LUVEG_8 * coefficients[12,] + pred$Mean_Patch_areaKm2_Veg_LU * coefficients[13,] + pred$SandySoils_withoutAbroad * coefficients[14,]

# calculate differences between model prediction and binary stack
Pred_difs <- new_pred - WBdiv_LUVEG

# write rasters
writeRaster(new_pred_LUVEG, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/LM_pred_ens_LUVEG.tif", format="GTiff")
writeRaster(Pred_difs_LUVEG, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/Dif_LMpred_and_LUVEG_ens.tif", format="GTiff")

# plotting created rasters
plot(WBdiv_LUVEG)
plot(new_pred_LUVEG)
plot(Pred_difs-LUVEG)
# Author: Bastiaen Boekelo
# October 9, 2015
# Goal: gain regression coefficients of the non-collinear predictor variables of LUVEG

#######################################
########### LU ANALYSES ############
#######################################

rm(list=ls())
library(raster)

# List rasters
WBdiv_LU <- raster("D:/SDM/SDM_SecondaryProducts/Diversity_ens_LU.tif")
pred <- stack(list.files("D:/SDM/Input_Rasters_KM2/full_area/LU", full.names=T, pattern=".tif$"))
pred <- pred[[2:8]]
ALL <- stack(pred, WBdiv_LU)
ALL_df <- as.data.frame(ALL)
ALL_df <- na.omit(ALL_df)
names(ALL_df)


# FILL IN WHICH VARIABLES WILL BE USED - MUST BE THE SAME AS LM !!!
names_vars <- c("ED_total_ManNat", "Food_Availability", "Mean_Patch_areaKm2_BS", "NumClasses_Fragstat", "SandySoils_withoutAbroad", "Suitability")

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
    
    # DATA COMES FROM ALL_df_copy !!
    fit <- lm(Diversity_ens_LU ~ ED_total_ManNat + Food_Availability + Mean_Patch_areaKm2_BS + NumClasses_Fragstat + SandySoils_withoutAbroad + Suitability, data=ALL_df_copy)
    
    # Getting r2
    r2 <- summary(fit)$r.squared
    
    # filling it into r2 table
    R2_table[i,ii] <- r2
    
  }
}

# CHANGE NAME TO SOMETHING GOOD !!!!
write.csv(R2_table, "D:/SDM/SDM_SecondaryProducts/Var_Imp_Diversity/R2_LU.csv", row.names=F)



# Multilinear Regression (after removel ED_total_man_nat)
fit <- lm(Diversity_ens_LU ~ ED_total_ManNat + Food_Availability + Mean_Patch_areaKm2_BS + NumClasses_Fragstat + SandySoils_withoutAbroad + Suitability, data=ALL_df)


# Getting parameter coefficients
coefficients <- as.data.frame(coef(fit))
coefficients
r2 <- summary(fit)$r.squared
r2

# Predicting new raster based on value
new_pred_LU <- pred$ED_total_ManNat*0 + coefficients[1,] + pred$ED_total_ManNat * coefficients[2,] + pred$Food_Availability * coefficients[3,] + pred$Mean_Patch_areaKm2_BS * coefficients[4,] + pred$NumClasses_Fragstat * coefficients[5,] + pred$SandySoils_withoutAbroad * coefficients[6,] + pred$Suitability * coefficients[7,]

# calculate differences between model prediction and binary stack
Pred_difs_LU <- new_pred_LU - WBdiv_LU

# write rasters
writeRaster(new_pred_LU, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/LM_pred_ens_LU.tif", format="GTiff", overwrite=T)
writeRaster(Pred_difs_LU, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/Dif_LMpred_and_LU_ens.tif", format="GTiff", overwrite=T)

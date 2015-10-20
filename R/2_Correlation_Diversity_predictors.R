# Author: Bastiaen Boekelo
# October 9, 2015
# Goal: gain regression coefficients of the non-collinear predictor variables

#######################################
########### LUVEG ANALYSES ############
#######################################

rm(list=ls())

library(raster)

# List rasters

WBdiv_LUVEG <- raster("D:/SDM/SDM_SecondaryProducts/Diversity_ens_LUVEG.tif")
pred_LUVEG <- stack(list.files("D:/SDM/Input_Rasters_KM2/full_area/LUVEG", full.names=T, pattern=".tif$"))
pred_LUVEG <- pred_LUVEG[[2:18]]
ALL <- stack(pred_LUVEG, WBdiv_LUVEG)
ALL_df <- as.data.frame(ALL)
names(ALL_df)

# Check for collinearity with scatters
#pairs(ALL)

# Multilinear Regression (after removel 2 edge densities, food availability and )
fit <- lm(Diversity_ens_LUVEG ~ LUVEG_1 + LUVEG_2 + LUVEG_20 + LUVEG_3 + LUVEG_30 + LUVEG_4 + LUVEG_5 + LUVEG_6 + LUVEG_60 + LUVEG_7 + LUVEG_8 + Mean_Patch_areaKm2_Veg_LU + SandySoils_withoutAbroad, data=ALL_df)

# fit <- lm(Diversity_ens_LUVEG ~ Food_Availability + LUVEG_1 + LUVEG_2 + LUVEG_20 + LUVEG_3 + LUVEG_30 + LUVEG_4 + LUVEG_5 + LUVEG_6 + LUVEG_60 + LUVEG_7 + LUVEG_8 + Mean_Patch_areaKm2_Veg_LU + SandySoils_withoutAbroad + Suitability + ED_total_6Classes + ED_total_ManNat, data=ALL_df)

# Getting parameter coefficients
coefficients <- as.data.frame(coef(fit))
coefficients
summary(fit)$r.squared

# Predicting new raster based on value
new_pred_LUVEG <- pred$LUVEG_1*0 + coefficients[1,] + pred$LUVEG_1 * coefficients[2,] + pred$LUVEG_2 * coefficients[3,] + pred$LUVEG_20 * coefficients[4,] + pred$LUVEG_3 * coefficients[5,] + pred$LUVEG_30 * coefficients[6,] + pred$LUVEG_4 * coefficients[7,] + pred$LUVEG_5 * coefficients[8,] + pred$LUVEG_6 * coefficients[9,] + pred$LUVEG_60 * coefficients[10,] + pred$LUVEG_7 * coefficients[11,] + pred$LUVEG_8 * coefficients[12,] + pred$Mean_Patch_areaKm2_Veg_LU * coefficients[13,] + pred$SandySoils_withoutAbroad * coefficients[14,]

# calculate differences between model prediction and binary stack
Pred_difs_LUVEG <- new_pred_LUVEG - WBdiv_LUVEG

# write rasters
writeRaster(new_pred_LUVEG, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/LM_pred_ens_LUVEG.tif", format="GTiff")
writeRaster(Pred_difs_LUVEG, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/Dif_LMpred_and_LUVEG_ens.tif", format="GTiff")

# plotting created rasters
plot(WBdiv_LUVEG)
plot(new_pred_LUVEG)
plot(Pred_difs-LUVEG)

#######################################
######### LU-VEG DIFFERENCES ##########
#######################################

# Load rasters
LU_ens <- raster("D:/SDM/SDM_SecondaryProducts/Diversity_ens_LU.tif")
VEG_ens <- raster("D:/SDM/SDM_SecondaryProducts/Diversity_ens_VEG.tif")

# Calculate and plot differences
LU_min_VEG <- LU_ens - VEG_ens
plot(LU_min_VEG)

# combine LUVEG predictors with difference layer
LU_and_LUVEGpredictors <- as.data.frame(stack(LU_min_VEG, pred))
pairs(LU_and_LUVEGpredictors)

# Regression with differences and predictors
div_fit <- lm(layer ~ LUVEG_1 + LUVEG_2 + LUVEG_20 + LUVEG_3 + LUVEG_30 + LUVEG_4 + LUVEG_5 + LUVEG_6 + LUVEG_60 + LUVEG_7 + LUVEG_8 + Mean_Patch_areaKm2_Veg_LU + SandySoils_withoutAbroad, data=LU_and_LUVEGpredictors)

# Getting parameter coefficients
coefficients <- as.data.frame(coef(div_fit))
coefficients

#######################################
######### LU PREDICTIONS ##########
#######################################


# List rasters

WBdiv_LU <- raster("D:/SDM/SDM_SecondaryProducts/Diversity_ens_LU.tif")
pred <- stack(list.files("D:/SDM/Input_Rasters_KM2/full_area/LU", full.names=T, pattern=".tif$"))
pred <- pred[[2:8]]
ALL <- stack(pred, WBdiv_LU)
ALL_df <- as.data.frame(ALL)
names(ALL_df)

# Check for collinearity with scatters
scatter <- pairs(ALL)

# Multilinear Regression (after removel ED_total_man_nat)
fit_LU <- lm(Diversity_ens_LU ~ ED_total_ManNat + Food_Availability + Mean_Patch_areaKm2_BS + NumClasses_Fragstat + SandySoils_withoutAbroad + Suitability, data=ALL_df)


# Getting parameter coefficients
coefficients <- as.data.frame(coef(fit_LU))
coefficients

# Predicting new raster based on value
new_pred_LU <- pred$ED_total_ManNat*0 + coefficients[1,] + pred$ED_total_ManNat * coefficients[2,] + pred$Food_Availability * coefficients[3,] + pred$Mean_Patch_areaKm2_BS * coefficients[4,] + pred$NumClasses_Fragstat * coefficients[5,] + pred$SandySoils_withoutAbroad * coefficients[6,] + pred$Suitability * coefficients[7,]

# calculate differences between model prediction and binary stack
Pred_difs_LU <- new_pred_LU - WBdiv_LU

# write rasters
writeRaster(new_pred_LU, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/LM_pred_ens_LU.tif", format="GTiff", overwrite=T)
writeRaster(Pred_difs_LU, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/Dif_LMpred_and_LU_ens.tif", format="GTiff", overwrite=T)

DIFF <- new_pred_LU - new_pred_LUVEG
plot(DIFF)
# plotting created rasters
plot(WBdiv_LU)
plot(new_pred_LU)
plot(Pred_difs_LU)
RSE <- sqrt(Pred_difs_LU^2)
RMSE <- cellStats(RSE, mean, na.omit=T)
summary(fit)$r.squared


#######################################
######### VEG PREDICTIONS ##########
#######################################


# List rasters

WBdiv_VEG <- raster("D:/SDM/SDM_SecondaryProducts/Diversity_ens_VEG.tif")
pred_VEG <- stack(list.files("D:/SDM/Input_Rasters_KM2/full_area/VEG", full.names=T, pattern=".tif$"))
pred_VEG <- pred_VEG[[2:12]]
ALL <- stack(pred_VEG, WBdiv_VEG)
ALL_df <- as.data.frame(ALL)
names(ALL_df)

# Check for collinearity with scatters
#scatter <- pairs(ALL)

# Multilinear Regression 
fit <- lm(Diversity_ens_VEG ~ EdgeDensity + Food_Availability + Mean_Patch_areaKm2_VEG_All + VEG_All_1 + VEG_All_2 + VEG_All_3 + VEG_All_4 + VEG_All_5 + VEG_All_6 + VEG_All_7 + VEG_All_8, data=ALL_df)

# Getting parameter coefficients
coefficients <- as.data.frame(coef(fit))
coefficients

# Predicting new raster based on value
new_pred_VEG <- pred_VEG$EdgeDensity*0 + coefficients[1,] + pred_VEG$EdgeDensity * coefficients[2,] + pred_VEG$Food_Availability * coefficients[3,] + pred_VEG$Mean_Patch_areaKm2_VEG_All * coefficients[4,] + pred_VEG$VEG_All_1 * coefficients[5,] + pred_VEG$VEG_All_2 * coefficients[6,] + pred_VEG$VEG_All_3 * coefficients[7,] + pred_VEG$VEG_All_4 * coefficients[8,] + pred_VEG$VEG_All_5 * coefficients[9,] + pred_VEG$VEG_All_6 * coefficients[10,] + pred_VEG$VEG_All_7 * coefficients[11,]

plot(new_pred_VEG)

# calculate differences between model prediction and binary stack
Pred_difs_VEG <- new_pred_VEG - WBdiv_VEG

# write rasters
writeRaster(new_pred_VEG, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/LM_pred_ens_VEG.tif", format="GTiff")
writeRaster(Pred_difs_VEG, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/Dif_LMpred_and_VEG_ens.tif", format="GTiff")

DIFF <- new_pred_LU - new_pred_VEG
plot(DIFF)
writeRaster(DIFF, "D:/SDM/SDM_SecondaryProducts/Diversity_analyses/Dif_LMpred_LU_minus_LMpred_VEG.tif", format="GTiff")

#################################################################
#### CORRELATING PREDICTED DIFFERENCES WITH VEG VARIABLES #######
#################################################################

LU_minus_VEG_and_predVEG <- stack(DIFF, pred_VEG)
scatter <- pairs(LU_minus_VEG_and_predVEG)
pairs(LU_minus_VEG_and_predVEG)

plot(





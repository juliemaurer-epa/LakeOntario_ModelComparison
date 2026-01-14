# Calculating MvM statistics
# requires dataframes generated from MvM_TP_FVCOM_EFDC_LOEM.R and MvM_temperature.R
# updated and streamlined version of archive/Rstats_Lake_Ontario_JM.R
# last modified 10/30/25

# Stowe et al. 2009 - suggests log transforming before statistics so that differences between actual and predicted are not skewed by small portion of outliers

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/")

# packages
library(readxl)
library(Metrics)
library(ggplot2)
library(tidyr)
library(dplyr)

# functions
source("../../scripts/skillstat_FUN.R")

################################################################################
#                         EFDC-TP vs FVCOM-TP vs LOEM
################################################################################

#### loading data and combining tables ####
mvm_scen1_fvcom <- read.csv("scenario1/Modeled-v-Measured_TP_2018_FVCOM_scenario1.csv", header = TRUE) # retain all column names here

mvm_scen1_efdc <- read.csv("scenario1/Modeled-v-Measured_TP_2018_EFDC_scenario1.csv", header = TRUE) %>% 
  select(date_time, Longitude, Latitude, node_fvcom, sigma, node_EFDC, K_Index, sample_depth, water_depth, TP_ugL.1, modelvalue)

mvm_scen1_loem <- read.csv("scenario1/Modeled-v-Measured_TP_2018_LOEM_scenario1.csv", header = TRUE) %>% 
  select(date_time, Longitude, Latitude, node_fvcom, sigma, node_EFDC, K_Index, sample_depth, water_depth, TP_ugL.1, modelvalue)

mvm_scen2_fvcom <- read.csv("scenario2/Modeled-v-Measured_TP_2018_FVCOM_scenario2.csv", header = TRUE) %>% 
  select(date_time, Longitude, Latitude, node_fvcom, sigma, node_EFDC, K_Index, sample_depth, water_depth, TP_ugL.1, modelvalue)

mvm_scen2_efdc <- read.csv("scenario2/Modeled-v-Measured_TP_2018_EFDC_scenario2.csv", header = TRUE) %>% 
  select(date_time, Longitude, Latitude, node_fvcom, sigma, node_EFDC, K_Index, sample_depth, water_depth, TP_ugL.1, modelvalue)

mvm_scen2_loem<- read.csv("scenario2/Modeled-v-Measured_TP_2018_LOEM_scenario2.csv", header = TRUE) %>% 
  select(date_time, Longitude, Latitude, node_fvcom, sigma, node_EFDC, K_Index, sample_depth, water_depth, TP_ugL.1, modelvalue)

mvm_scen3_efdc <- read.csv("scenario3/Modeled-v-Measured_TP_2018_EFDC_scenario3.csv", header = TRUE) %>% 
  select(date_time, Longitude, Latitude, node_fvcom, sigma, node_EFDC, K_Index, sample_depth, water_depth, TP_ugL.1, modelvalue)

mvm_scen3_loem <- read.csv("scenario3/Modeled-v-Measured_TP_2018_LOEM_scenario3.csv", header = TRUE) %>% 
  select(date_time, Longitude, Latitude, node_fvcom, sigma, node_EFDC, K_Index, sample_depth, water_depth, TP_ugL.1, modelvalue)

# combining MvM tables
mvm_TP <- mvm_scen1_fvcom %>%
  rename(FVCOM_scen1 = modelvalue) %>%
  left_join(mvm_scen1_efdc, # add EFDC scenario 1
            by = c("date_time", "Longitude", "Latitude", "node_fvcom", "sigma", 
                   "node_EFDC", "K_Index", "sample_depth", "water_depth", "TP_ugL.1")) %>% #join by all except model value 
  rename(EFDC_scen1 = modelvalue) %>%
  left_join(mvm_scen1_loem, # add LOEM scenario 1
            by = c("date_time", "Longitude", "Latitude", "node_fvcom", "sigma", 
                   "node_EFDC", "K_Index", "sample_depth", "water_depth", "TP_ugL.1")) %>% 
  rename(LOEM_scen1 = modelvalue) %>%
  left_join(mvm_scen2_fvcom, # add FVCOM scenario 2
            by = c("date_time", "Longitude", "Latitude", "node_fvcom", "sigma", 
                   "node_EFDC", "K_Index", "sample_depth", "water_depth", "TP_ugL.1")) %>% 
  rename(FVCOM_scen2 = modelvalue) %>%
  left_join(mvm_scen2_efdc, # add EFDC scenario 2
            by = c("date_time", "Longitude", "Latitude", "node_fvcom", "sigma", 
                   "node_EFDC", "K_Index", "sample_depth", "water_depth", "TP_ugL.1")) %>% 
  rename(EFDC_scen2 = modelvalue) %>%
  left_join(mvm_scen2_loem, # add LOEM scenario 2
            by = c("date_time", "Longitude", "Latitude", "node_fvcom", "sigma", 
                   "node_EFDC", "K_Index", "sample_depth", "water_depth", "TP_ugL.1")) %>% 
  rename(LOEM_scen2 = modelvalue) %>%
  left_join(mvm_scen3_efdc, # add EFDC scenario 3
            by = c("date_time", "Longitude", "Latitude", "node_fvcom", "sigma", 
                   "node_EFDC", "K_Index", "sample_depth", "water_depth", "TP_ugL.1")) %>% 
  rename(EFDC_scen3 = modelvalue) %>%
  left_join(mvm_scen3_loem, # add LOEM scenario 3
            by = c("date_time", "Longitude", "Latitude", "node_fvcom", "sigma", 
                   "node_EFDC", "K_Index", "sample_depth", "water_depth", "TP_ugL.1")) %>% 
  rename(LOEM_scen3 = modelvalue,
         Real_TP = TP_ugL.1) %>%
  select(date_time, Longitude, Latitude, node_fvcom, sigma, node_EFDC, I_Index, #order relevant columns
         J_Index, K_Index, sample_depth, water_depth, Real_TP, FVCOM_scen1,
         EFDC_scen1, LOEM_scen1, FVCOM_scen2, EFDC_scen2, LOEM_scen2, 
         EFDC_scen3, LOEM_scen3) %>%
  mutate(depth_interval = cut(water_depth, breaks = c(0,5,30,max(water_depth)), #add depth intervals
                              labels = c("<5m", "5-30m", ">30m")))

summary(is.na(mvm_TP)) # 2 NAs in LOEM results - drop these rows from the comparison
mvm_TP2 <- drop_na(mvm_TP)
summary(is.na(mvm_TP2))

write.csv(mvm_TP2, "MvM_TP_combined.csv")

#### Calculate statistics ####
## whole lake
realTP <- mvm_TP2$Real_TP
pred_FVCOM_1 <- mvm_TP2$FVCOM_scen1
pred_EFDC_1 <- mvm_TP2$EFDC_scen1
pred_LOEM_1 <- mvm_TP2$LOEM_scen1
pred_FVCOM_2 <- mvm_TP2$FVCOM_scen2
pred_EFDC_2 <- mvm_TP2$EFDC_scen2
pred_LOEM_2 <- mvm_TP2$LOEM_scen2
pred_EFDC_3 <- mvm_TP2$EFDC_scen3
pred_LOEM_3 <- mvm_TP2$LOEM_scen3

#RMSE: rmse(actual, predicted) - root mean squared error between two numeric vectors
RMSE_TP <- c(
  RMSE_FVCOM_1 <- rmse(realTP, pred_FVCOM_1),
  RMSE_EFDC_1 <- rmse(realTP, pred_EFDC_1),
  RMSE_LOEM_1 <- rmse(realTP, pred_LOEM_1),
  RMSE_FVCOM_2 <- rmse(realTP, pred_FVCOM_2),
  RMSE_EFDC_2 <- rmse(realTP, pred_EFDC_2),
  RMSE_LOEM_2 <- rmse(realTP, pred_LOEM_2),
  RMSE_EFDC_3 <- rmse(realTP, pred_EFDC_3),
  RMSE_LOEM_3 <- rmse(realTP, pred_LOEM_3)
  )

#MAE: mae(actual, predicted) - average absolute difference between two vectors
MAE_TP <- c(
  MAE_FVCOM_1 <- mae(realTP, pred_FVCOM_1),
  MAE_EFDC_1 <- mae(realTP, pred_EFDC_1),
  MAE_LOEM_1 <- mae(realTP, pred_LOEM_1),
  MAE_FVCOM_2 <- mae(realTP, pred_FVCOM_2),
  MAE_EFDC_2 <- mae(realTP, pred_EFDC_2),
  MAE_LOEM_2 <- mae(realTP, pred_LOEM_2),
  MAE_EFDC_3 <- mae(realTP, pred_EFDC_3),
  MAE_LOEM_3 <- mae(realTP, pred_LOEM_3)
  )

#BIAS: bias(actual, predicted) - average amount by which actual is greater than predicted
BIAS_TP <- c(
  BIAS_FVCOM_1 <- bias(realTP, pred_FVCOM_1),
  BIAS_EFDC_1 <- bias(realTP, pred_EFDC_1),
  BIAS_LOEM_1 <- bias(realTP, pred_LOEM_1),
  BIAS_FVCOM_2 <- bias(realTP, pred_FVCOM_2),
  BIAS_EFDC_2 <- bias(realTP, pred_EFDC_2),
  BIAS_LOEM_2 <- bias(realTP, pred_LOEM_2),
  BIAS_EFDC_3 <- bias(realTP, pred_EFDC_3),
  BIAS_LOEM_3 <- bias(realTP, pred_LOEM_3)
  )

#SKILL: skill(predicted, actual)
SKILL_TP <- c(
  SKILL_FVCOM_1 <- skill.stat(pred_FVCOM_1, realTP),
  SKILL_EFDC_1 <- skill.stat(pred_EFDC_1, realTP),
  SKILL_LOEM_1 <- skill.stat(pred_LOEM_1, realTP),
  SKILL_FVCOM_2 <- skill.stat(pred_FVCOM_2, realTP),
  SKILL_EFDC_2 <- skill.stat(pred_EFDC_2, realTP),
  SKILL_LOEM_2 <- skill.stat(pred_LOEM_2, realTP),
  SKILL_EFDC_3 <- skill.stat(pred_EFDC_3, realTP),
  SKILL_LOEM_3 <- skill.stat(pred_LOEM_3, realTP)
  )

#### very nearshore (< 5m)
realTP_5m <- mvm_TP2$Real_TP[mvm_TP2$water_depth < 5]
pred_FVCOM1_5m <- mvm_TP2$FVCOM_scen1[mvm_TP2$water_depth < 5]
pred_EFDC1_5m <- mvm_TP2$EFDC_scen1[mvm_TP2$water_depth < 5]
pred_LOEM1_5m <- mvm_TP2$LOEM_scen1[mvm_TP2$water_depth < 5]
pred_FVCOM2_5m <- mvm_TP2$FVCOM_scen2[mvm_TP2$water_depth < 5]
pred_EFDC2_5m <- mvm_TP2$EFDC_scen2[mvm_TP2$water_depth < 5]
pred_LOEM2_5m <- mvm_TP2$LOEM_scen2[mvm_TP2$water_depth < 5]
pred_EFDC3_5m <- mvm_TP2$EFDC_scen3[mvm_TP2$water_depth < 5]
pred_LOEM3_5m <- mvm_TP2$LOEM_scen3[mvm_TP2$water_depth < 5]

#RMSE - rmse(actual, predicted)
RMSE_TP_5m <- c(
  RMSE_FVCOM1_5m <- rmse(realTP_5m, pred_FVCOM1_5m),
  RMSE_EFDC1_5m <- rmse(realTP_5m, pred_EFDC1_5m),
  RMSE_LOEM1_5m <- rmse(realTP_5m, pred_LOEM1_5m),
  RMSE_FVCOM2_5m <- rmse(realTP_5m, pred_FVCOM2_5m),
  RMSE_EFDC2_5m <- rmse(realTP_5m, pred_EFDC2_5m),
  RMSE_LOEM2_5m <- rmse(realTP_5m, pred_LOEM2_5m),
  RMSE_EFDC3_5m <- rmse(realTP_5m, pred_EFDC3_5m),
  RMSE_LOEM3_5m <- rmse(realTP_5m, pred_LOEM3_5m)
)

#MAE - mae(actual, predicted)
MAE_TP_5m <- c(
  MAE_FVCOM1_5m <- mae(realTP_5m, pred_FVCOM1_5m),
  MAE_EFDC1_5m <- mae(realTP_5m, pred_EFDC1_5m),
  MAE_LOEM1_5m <- mae(realTP_5m, pred_LOEM1_5m),
  MAE_FVCOM2_5m <- mae(realTP_5m, pred_FVCOM2_5m),
  MAE_EFDC2_5m <- mae(realTP_5m, pred_EFDC2_5m),
  MAE_LOEM2_5m <- mae(realTP_5m, pred_LOEM2_5m),
  MAE_EFDC3_5m <- mae(realTP_5m, pred_EFDC3_5m),
  MAE_LOEM3_5m <- mae(realTP_5m, pred_LOEM3_5m)
)

#BIAS - bias(actual, predicted)
BIAS_TP_5m <- c(
  BIAS_FVCOM1_5m <- bias(realTP_5m, pred_FVCOM1_5m),
  BIAS_EFDC1_5m <- bias(realTP_5m, pred_EFDC1_5m),
  BIAS_LOEM1_5m <- bias(realTP_5m, pred_LOEM1_5m),
  BIAS_FVCOM2_5m <- bias(realTP_5m, pred_FVCOM2_5m),
  BIAS_EFDC2_5m <- bias(realTP_5m, pred_EFDC2_5m),
  BIAS_LOEM2_5m <- bias(realTP_5m, pred_LOEM2_5m),
  BIAS_EFDC3_5m <- bias(realTP_5m, pred_EFDC3_5m),
  BIAS_LOEM3_5m <- bias(realTP_5m, pred_LOEM3_5m)
  )

#SKILL 2018 skill(predicted, actual)
SKILL_TP_5m <- c(
  SKILL_FVCOM1_5m <- skill.stat(pred_FVCOM1_5m, realTP_5m),
  SKILL_EFDC1_5m <- skill.stat(pred_EFDC1_5m, realTP_5m),
  SKILL_LOEM1_5m <- skill.stat(pred_LOEM1_5m, realTP_5m),
  SKILL_FVCOM2_5m <- skill.stat(pred_FVCOM2_5m, realTP_5m),
  SKILL_EFDC2_5m <- skill.stat(pred_EFDC2_5m, realTP_5m),
  SKILL_LOEM2_5m <- skill.stat(pred_EFDC2_5m, realTP_5m),
  SKILL_EFDC3_5m <- skill.stat(pred_EFDC3_5m, realTP_5m),
  SKILL_LOEM3_5m <- skill.stat(pred_LOEM3_5m, realTP_5m)
  )

### nearshore (5 - 30m)
realTP_5_30m <- mvm_TP2$Real_TP[mvm_TP2$water_depth > 5 & 
                                  mvm_TP2$water_depth < 30]
pred_FVCOM1_5_30m <- mvm_TP2$FVCOM_scen1[mvm_TP2$water_depth > 5 & 
                                        mvm_TP2$water_depth < 30]
pred_EFDC1_5_30m <- mvm_TP2$EFDC_scen1[mvm_TP2$water_depth > 5 & 
                                      mvm_TP2$water_depth < 30]
pred_LOEM1_5_30m <- mvm_TP2$LOEM_scen1[mvm_TP2$water_depth > 5 & 
                                      mvm_TP2$water_depth < 30]
pred_FVCOM2_5_30m <- mvm_TP2$FVCOM_scen2[mvm_TP2$water_depth > 5 & 
                                        mvm_TP2$water_depth < 30]
pred_EFDC2_5_30m <- mvm_TP2$EFDC_scen2[mvm_TP2$water_depth > 5 & 
                                     mvm_TP2$water_depth < 30]
pred_LOEM2_5_30m <- mvm_TP2$LOEM_scen2[mvm_TP2$water_depth > 5 & 
                                      mvm_TP2$water_depth < 30]
pred_EFDC3_5_30m <- mvm_TP2$EFDC_scen3[mvm_TP2$water_depth > 5 & 
                                      mvm_TP2$water_depth < 30]
pred_LOEM3_5_30m <- mvm_TP2$LOEM_scen3[mvm_TP2$water_depth > 5 & 
                                      mvm_TP2$water_depth < 30]

#RMSE - rmse(actual, predicted)
RMSE_TP_5_30m <- c(
  RMSE_FVCOM1_5_30m <- rmse(realTP_5_30m, pred_FVCOM1_5_30m),
  RMSE_EFDC1_5_30m <- rmse(realTP_5_30m, pred_EFDC1_5_30m),
  RMSE_LOEM1_5_30m <- rmse(realTP_5_30m, pred_LOEM1_5_30m),
  RMSE_FVCOM2_5_30m <- rmse(realTP_5_30m, pred_FVCOM2_5_30m),
  RMSE_EFDC2_5_30m <- rmse(realTP_5_30m, pred_EFDC2_5_30m),
  RMSE_LOEM2_5_30m <- rmse(realTP_5_30m, pred_LOEM2_5_30m),
  RMSE_EFDC3_5_30m <- rmse(realTP_5_30m, pred_EFDC3_5_30m),
  RMSE_LOEM3_5_30m <- rmse(realTP_5_30m, pred_LOEM3_5_30m)
)

#MAE - mae(actual, predicted)
MAE_TP_5_30m <- c(
  MAE_FVCOM1_5_30m <- mae(realTP_5_30m, pred_FVCOM1_5_30m),
  MAE_EFDC1_5_30m <- mae(realTP_5_30m, pred_EFDC1_5_30m),
  MAE_LOEM1_5_30m <- mae(realTP_5_30m, pred_LOEM1_5_30m),
  MAE_FVCOM2_5_30m <- mae(realTP_5_30m, pred_FVCOM2_5_30m),
  MAE_EFDC2_5_30m <- mae(realTP_5_30m, pred_EFDC2_5_30m),
  MAE_LOEM2_5_30m <- mae(realTP_5_30m, pred_LOEM2_5_30m),
  MAE_EFDC3_5_30m <- mae(realTP_5_30m, pred_EFDC3_5_30m),
  MAE_LOEM3_5_30m <- mae(realTP_5_30m, pred_LOEM3_5_30m)
)

#BIAS - bias(actual, predicted)
BIAS_TP_5_30m <- c(
  BIAS_FVCOM1_5_30m <- bias(realTP_5_30m, pred_FVCOM1_5_30m),
  BIAS_EFDC1_5_30m <- bias(realTP_5_30m, pred_EFDC1_5_30m),
  BIAS_LOEM1_5_30m <- bias(realTP_5_30m, pred_LOEM1_5_30m),
  BIAS_FVCOM2_5_30m <- bias(realTP_5_30m, pred_FVCOM2_5_30m),
  BIAS_EFDC2_5_30m <- bias(realTP_5_30m, pred_EFDC2_5_30m),
  BIAS_LOEM2_5_30m <- bias(realTP_5_30m, pred_LOEM2_5_30m),
  BIAS_EFDC3_5_30m <- bias(realTP_5_30m, pred_EFDC3_5_30m),
  BIAS_LOEM3_5_30m <- bias(realTP_5_30m, pred_LOEM3_5_30m)
)

#SKILL 2018 skill(predicted, actual)
SKILL_TP_5_30m <- c(
  SKILL_FVCOM1_5_30m <- skill.stat(pred_FVCOM1_5_30m, realTP_5_30m),
  SKILL_EFDC1_5_30m <- skill.stat(pred_EFDC1_5_30m, realTP_5_30m),
  SKILL_LOEM1_5_30m <- skill.stat(pred_LOEM1_5_30m, realTP_5_30m),
  SKILL_FVCOM2_5_30m <- skill.stat(pred_FVCOM2_5_30m, realTP_5_30m),
  SKILL_EFDC2_5_30m <- skill.stat(pred_EFDC2_5_30m, realTP_5_30m),
  SKILL_LOEM2_5_30m <- skill.stat(pred_LOEM2_5_30m, realTP_5_30m),
  SKILL_EFDC3_5_30m <- skill.stat(pred_EFDC3_5_30m, realTP_5_30m),
  SKILL_LOEM3_5_30m <- skill.stat(pred_LOEM3_5_30m, realTP_5_30m)
)

#### Offshore (> 30m)
realTP_30m <- mvm_TP2$Real_TP[mvm_TP2$water_depth > 30]
pred_FVCOM1_30m <- mvm_TP2$FVCOM_scen1[mvm_TP2$water_depth > 30]
pred_EFDC1_30m <- mvm_TP2$EFDC_scen1[mvm_TP2$water_depth > 30]
pred_LOEM1_30m <- mvm_TP2$LOEM_scen1[mvm_TP2$water_depth > 30]
pred_FVCOM2_30m <- mvm_TP2$FVCOM_scen2[mvm_TP2$water_depth > 30]
pred_EFDC2_30m <- mvm_TP2$EFDC_scen2[mvm_TP2$water_depth > 30]
pred_LOEM2_30m <- mvm_TP2$LOEM_scen2[mvm_TP2$water_depth > 30]
pred_EFDC3_30m <- mvm_TP2$EFDC_scen3[mvm_TP2$water_depth > 30]
pred_LOEM3_30m <- mvm_TP2$LOEM_scen3[mvm_TP2$water_depth > 30]

#RMSE - rmse(actual, predicted)
RMSE_TP_30m <- c(
  RMSE_FVCOM1_30m <- rmse(realTP_30m, pred_FVCOM1_30m),
  RMSE_EFDC1_30m <- rmse(realTP_30m, pred_EFDC1_30m),
  RMSE_LOEM1_30m <- rmse(realTP_30m, pred_LOEM1_30m),
  RMSE_FVCOM2_30m <- rmse(realTP_30m, pred_FVCOM2_30m),
  RMSE_EFDC2_30m <- rmse(realTP_30m, pred_EFDC2_30m),
  RMSE_LOEM2_30m <- rmse(realTP_30m, pred_LOEM2_30m),
  RMSE_EFDC3_30m <- rmse(realTP_30m, pred_EFDC3_30m),
  RMSE_LOEM3_30m <- rmse(realTP_30m, pred_LOEM3_30m)
)

#MAE - mae(actual, predicted)
MAE_TP_30m <- c(
  MAE_FVCOM1_30m <- mae(realTP_30m, pred_FVCOM1_30m),
  MAE_EFDC1_30m <- mae(realTP_30m, pred_EFDC1_30m),
  MAE_LOEM1_30m <- mae(realTP_30m, pred_LOEM1_30m),
  MAE_FVCOM2_30m <- mae(realTP_30m, pred_FVCOM2_30m),
  MAE_EFDC2_30m <- mae(realTP_30m, pred_EFDC2_30m),
  MAE_LOEM2_30m <- mae(realTP_30m, pred_LOEM2_30m),
  MAE_EFDC3_30m <- mae(realTP_30m, pred_EFDC3_30m),
  MAE_LOEM3_30m <- mae(realTP_30m, pred_LOEM3_30m)
)

#BIAS - bias(actual, predicted)
BIAS_TP_30m <- c(
  BIAS_FVCOM1_30m <- bias(realTP_30m, pred_FVCOM1_30m),
  BIAS_EFDC1_30m <- bias(realTP_30m, pred_EFDC1_30m),
  BIAS_LOEM1_30m <- bias(realTP_30m, pred_LOEM1_30m),
  BIAS_FVCOM2_30m <- bias(realTP_30m, pred_FVCOM2_30m),
  BIAS_EFDC2_30m <- bias(realTP_30m, pred_EFDC2_30m),
  BIAS_LOEM2_30m <- bias(realTP_30m, pred_LOEM2_30m),
  BIAS_EFDC3_30m <- bias(realTP_30m, pred_EFDC3_30m),
  BIAS_LOEM3_30m <- bias(realTP_30m, pred_LOEM3_30m)
)

#SKILL 2018 skill(predicted, actual)
SKILL_TP_30m <- c(
  SKILL_FVCOM1_30m <- skill.stat(pred_FVCOM1_30m, realTP_30m),
  SKILL_EFDC1_30m <- skill.stat(pred_EFDC1_30m, realTP_30m),
  SKILL_LOEM1_30m <- skill.stat(pred_LOEM1_30m, realTP_30m),
  SKILL_FVCOM2_30m <- skill.stat(pred_FVCOM2_30m, realTP_30m),
  SKILL_EFDC2_30m <- skill.stat(pred_EFDC2_30m, realTP_30m),
  SKILL_LOEM2_30m <- skill.stat(pred_EFDC2_30m, realTP_30m),
  SKILL_EFDC3_30m <- skill.stat(pred_EFDC3_30m, realTP_30m),
  SKILL_LOEM3_30m <- skill.stat(pred_LOEM3_30m, realTP_30m)
)

#### Create statistics dataframe ####
Model <- c("FVCOM_scenario1", "EFDC_scenario1", "LOEM_scenario1",
           "FVCOM_scenario2", "EFDC_scenario2", "LOEM_scenario2",
           "EFDC_scenario3", "LOEM_scenario3")
TPstats <- data.frame(Model, RMSE_TP, RMSE_TP_5m, RMSE_TP_5_30m, RMSE_TP_30m,
                      MAE_TP, MAE_TP_5m, MAE_TP_5_30m, MAE_TP_30m,
                      BIAS_TP, BIAS_TP_5m, BIAS_TP_5_30m, BIAS_TP_30m,
                      SKILL_TP, SKILL_TP_5m, SKILL_TP_5_30m, SKILL_TP_30m)
write.csv(TPstats, "/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/MvM_TP_statistics.csv")

################################################################################
#                           EFDC-Temp vs FVCOM-Temp
################################################################################

mvm_temp_efdc <- read.csv("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/temp/Modeled-v-Measured_Temp_2018_EFDC.csv", header = TRUE)

mvm_temp_fvcom <- read.csv("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/temp/Modeled-v-Measured_Temp_2018_FVCOM.csv", header = TRUE)

# make sure outputs match
nrow(mvm_temp_efdc)
nrow(mvm_temp_fvcom) #fvcom matched to 6 more datapoints than efdc...

#add modeled TP from each scenario to mvm_scenarios:
mvm_temp <- mvm_temp_efdc %>%
  rename(EFDC_temp = modelvalue) %>%
  left_join(mvm_temp_fvcom, 
            by = c("date_time", "lat.x", "lng", "type", "source", "siteID",
                   "siteDepth", "sampleDepth", "temp", "node_fvcom", "sigma", 
                   "node_efdc", "I_Index", "J_Index", "dist_fvcom", "dist_efdc",
                   "nLayers_efdc", "K_Index", "JulianDate")) %>%
  select(date_time, lat.x, lng, type, siteDepth, sampleDepth, temp, 
         node_fvcom, modelvalue, node_efdc, EFDC_temp) %>%
  rename(FVCOM_temp = modelvalue,
         Real_temp = temp)

# 1 NA in real temp
mvm_temp <- drop_na(mvm_temp)

write.csv(mvm_temp, "Statistics/temp/Modeled-v-Measured_Temp_2018_efdc-v-fvcom_formatted.csv")

#### Calculate statistics - whole lake
realtemp <- mvm_temp$Real_temp
predictedEFDC <- mvm_temp$EFDC_temp
predictedFVCOM <- mvm_temp$FVCOM_temp

#RMSE - rmse(actual, predicted)
RMSE_temp <- c(
  RMSE_EFDC <- rmse(realtemp, predictedEFDC),
  RMSE_FVCOM <- rmse(realtemp, predictedFVCOM))

#MAE - mae(actual, predicted)
MAE_temp <- c(
  MAE_EFDC <- mae(realtemp, predictedEFDC),
  MAE_FVCOM <- mae(realtemp, predictedFVCOM))

#BIAS - bias(actual, predicted)
BIAS_temp <- c(
  BIAS_EFDC <- bias(realtemp, predictedEFDC),
  BIAS_FVCOM <- bias(realtemp, predictedFVCOM))

#SKILL 2018 skill(predicted, actual)
SKILL_temp <- c(
  SKILL_EFDC <- skill.stat(predictedEFDC, realtemp),
  SKILL_FVCOM <- skill.stat(predictedFVCOM, realtemp))

#### very nearshore (< 5m)
realtemp_5m <- mvm_temp$Real_temp[mvm_temp$siteDepth < 5]
predictedEFDC_5m <- mvm_temp$EFDC_temp[mvm_temp$siteDepth < 5]
predictedFVCOM_5m <- mvm_temp$FVCOM_temp[mvm_temp$siteDepth < 5]

#RMSE - rmse(actual, predicted)
RMSE_temp_5m <- c(
  RMSE_EFDC_5m <- rmse(realtemp_5m, predictedEFDC_5m),
  RMSE_FVCOM_5m <- rmse(realtemp_5m, predictedFVCOM_5m))

#MAE - mae(actual, predicted)
MAE_temp_5m <- c(
  MAE_EFDC_5m <- mae(realtemp_5m, predictedEFDC_5m),
  MAE_FVCOM_5m <- mae(realtemp_5m, predictedFVCOM_5m))

#BIAS - bias(actual, predicted)
BIAS_temp_5m <- c(
  BIAS_EFDC_5m <- bias(realtemp_5m, predictedEFDC_5m),
  BIAS_FVCOM_5m <- bias(realtemp_5m, predictedFVCOM_5m))

#SKILL 2018 skill(predicted, actual)
SKILL_temp_5m <- c(
  SKILL_EFDC_5m <- skill.stat(predictedEFDC_5m, realtemp_5m),
  SKILL_FVCOM_5m <- skill.stat(predictedFVCOM_5m, realtemp_5m))

### nearshore (5 - 30m)
realtemp_5_30m <- mvm_temp$Real_temp[mvm_temp$siteDepth > 5 & 
                                       mvm_temp$siteDepth < 30]
predictedEFDC_5_30m <- mvm_temp$EFDC_temp[mvm_temp$siteDepth > 5 &
                                            mvm_temp$siteDepth < 30]
predictedFVCOM_5_30m <- mvm_temp$FVCOM_temp[mvm_temp$siteDepth > 5 &
                                              mvm_temp$siteDepth < 30]

#RMSE - rmse(actual, predicted)
RMSE_temp_5_30m <- c(
  RMSE_EFDC_5_30m <- rmse(realtemp_5_30m, predictedEFDC_5_30m),
  RMSE_FVCOM_5_30m <- rmse(realtemp_5_30m, predictedFVCOM_5_30m))

#MAE - mae(actual, predicted)
MAE_temp_5_30m <- c(
  MAE_EFDC_5_30m <- mae(realtemp_5_30m, predictedEFDC_5_30m),
  MAE_FVCOM_5_30m <- mae(realtemp_5_30m, predictedFVCOM_5_30m))

#BIAS - bias(actual, predicted)
BIAS_temp_5_30m <- c(
  BIAS_EFDC_5_30m <- bias(realtemp_5_30m, predictedEFDC_5_30m),
  BIAS_FVCOM_5_30m <- bias(realtemp_5_30m, predictedFVCOM_5_30m))

#SKILL 2018 skill(predicted, actual)
SKILL_temp_5_30m <- c(
  SKILL_EFDC_5_30m <- skill.stat(predictedEFDC_5_30m, realtemp_5_30m),
  SKILL_FVCOM_5_30m <- skill.stat(predictedFVCOM_5_30m, realtemp_5_30m))

#### Offshore (> 30m)
realtemp_30m <- mvm_temp$Real_temp[mvm_temp$siteDepth > 30]
predictedEFDC_30m <- mvm_temp$EFDC_temp[mvm_temp$siteDepth > 30]
predictedFVCOM_30m <- mvm_temp$FVCOM_temp[mvm_temp$siteDepth > 30]

#RMSE - rmse(actual, predicted)
RMSE_temp_30m <- c(
  RMSE_EFDC_30m <- rmse(realtemp_30m, predictedEFDC_30m),
  RMSE_FVCOM_30m <- rmse(realtemp_30m, predictedFVCOM_30m))

#MAE - mae(actual, predicted)
MAE_temp_30m <- c(
  MAE_EFDC_30m <- mae(realtemp_30m, predictedEFDC_30m),
  MAE_FVCOM_30m <- mae(realtemp_30m, predictedFVCOM_30m))

#BIAS - bias(actual, predicted)
BIAS_temp_30m <- c(
  BIAS_EFDC_30m <- bias(realtemp_30m, predictedEFDC_30m),
  BIAS_FVCOM_30m <- bias(realtemp_30m, predictedFVCOM_30m))

#SKILL 2018 skill(predicted, actual)
SKILL_temp_30m <- c(
  SKILL_EFDC_30m <- skill.stat(predictedEFDC_30m, realtemp_30m),
  SKILL_FVCOM_30m <- skill.stat(predictedFVCOM_30m, realtemp_30m))

#Putting the individual statistic dataframes together into one dataframe 
Model <- c("FVCOM", "EFDC")
tempstats <- data.frame(Model, RMSE_temp, MAE_temp, BIAS_temp, SKILL_temp,
                        RMSE_temp_5m, MAE_temp_5m, BIAS_temp_5m, SKILL_temp_5m,
                        RMSE_temp_5_30m, MAE_temp_5_30m, BIAS_temp_5_30m, 
                        SKILL_temp_5_30m, RMSE_temp_30m, MAE_temp_30m, BIAS_temp_30m,
                        SKILL_temp_30m)
tempstats

write.csv(tempstats, "Statistics/temp/temp_stats.csv")

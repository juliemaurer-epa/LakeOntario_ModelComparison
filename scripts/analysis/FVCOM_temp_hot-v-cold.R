# FVCOM temperature: hot vs cold start runs
# Date created: 11/13/25

library(ncdf4)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(Metrics)

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison")
output_folder <- "Statistics/temp/"

source("../scripts/model2data/nodematchFVCOM.R")
source("../scripts/model2data/skillstat_FUN.R")

########################### DO NOT RUN INTERACTIVELY ############################
## Model output

fvcom.cold <- "/work/GLHABS/wmelende/FVCOM_LO_Test/scenario1/output/fvcom_0001.nc"
nc1 <- nc_open(fvcom.cold)

time1 <- ncvar_get(nc1, varid = "time") #days since 1970-01-01
param1 <- ncvar_get(nc1, varid = "temp") 

minTime <- min(time1) 
maxTime <- max(time1)

fvcom.hot <- "/work/GLHABS/wmelende/FVCOM_LO_Test/temperature_test/output/fvcom_0001.nc"
nc2 <- nc_open(fvcom.hot)

time2 <- ncvar_get(nc2, varid = "time")
param2 <- ncvar_get(nc2, varid = "temp")

nc_close(nc1)
nc_close(nc2)

## station data
df <- read.csv("../TPData_processing/Temp_observationaldata_combined_loem-fvcom.csv") %>%
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y"),
         JulianDate = (as.numeric(date_time))/86400) %>%
  dplyr::select(-c(X, X.1, X.2)) %>%
  rename("lat" = lat.x)

# Only use field data within cold start run time period
df <- df[df$JulianDate >= minTime,]
df <- df[df$JulianDate <= maxTime,]

## Generate modeled vs measured Temp dataframes

# calculating nearest model value to measured values with function
modelvalue1 <- nodematchFVCOM(param1, time1, df)

dfCold <- df
dfCold$modelvalue <- modelvalue1
write.csv(dfCold, 
          file = file.path(output_folder, "Modeled-v-Measured_Temp_FVCOM_coldstart.csv", fsep = "/"), 
          row.names = FALSE) 

modelvalue2 <- nodematchFVCOM(param2, time2, df)

dfHot <- df
dfHot$modelvalue <- modelvalue2
write.csv(dfHot, 
          file = file.path(output_folder, "Modeled-v-Measured_Temp_FVCOM_hotstart.csv", fsep = "/"),
          row.names = FALSE)

##############################################################################

dfCold <- read.csv("Statistics/temp/Modeled-v-Measured_Temp_FVCOM_coldstart.csv")
dfHot <- read.csv("Statistics/temp/Modeled-v-Measured_Temp_FVCOM_hotstart.csv")

#### combine dataframes and compare ####

mvm_temp <- dfCold %>%
  left_join(dfHot, by = c("date_time", "lat", "lng", "type", "source",
                          "siteID", "siteDepth", "sampleDepth", "temp",
                          "node_fvcom", "sigma", "node_efdc", "I_Index",
                          "J_Index", "dist_fvcom", "dist_efdc", "nLayers_efdc",
                          "K_Index", "JulianDate")) %>%
  rename("temp_cold" = modelvalue.x,
         "temp_hot" = modelvalue.y,
         "real_temp" = temp) %>%
  mutate(date_time = as.Date(date_time, "%Y-%m-%d"))

mvm_temp_res <- mvm_temp %>%
  mutate(res_cold = temp_cold - real_temp,
         res_hot = temp_hot - real_temp) %>%
  pivot_longer(cols = c(res_cold, res_hot),
               names_to = "Run",
               values_to = "Residuals") %>%
  dplyr::select(date_time, lat, lng, node_fvcom, sigma, 
                siteDepth, sampleDepth, Run, Residuals, real_temp) 

# residual plots (residuals over time)
colorvar2 <- c("#E69F00", "#56B4E9")

p2 <- ggplot(data = mvm_temp_res, aes(x = date_time, y = Residuals, color = Run)) + 
  theme_classic() + geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 0, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(x = "Time", y = "Residuals (Modeled - Measured)") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        legend.position = "bottom")
p2
ggsave(plot = p2, filename = "plots/residuals_FVCOM_hot-v-cold_runs.png", width = 8, height = 6)

##### comparing hot start with EFDC ####
mvm_temp <- read.csv(
  "/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/temp/Modeled-v-Measured_Temp_2018_efdc-v-fvcom_formatted.csv") %>%
  dplyr::select(-c(X)) %>%
  rename("lat" = lat.x) %>%
  mutate(date_time = as.Date(date_time, "%Y-%m-%d"))

dfHot$date_time <- as.Date(dfHot$date_time, "%Y-%m-%d")

mvm_temp <- mvm_temp %>%
  rename("FVCOM_cold" = FVCOM_temp) %>%
  left_join(dfHot, by = c("date_time", "lat", "lng", "type", "siteDepth", 
                          "sampleDepth", "Real_temp" = "temp", "node_fvcom", 
                          "node_efdc")) %>%
  rename("FVCOM_hot" = modelvalue) 

mvm_temp2 <- mvm_temp %>%
  pivot_longer(cols = c(EFDC_temp, FVCOM_hot, FVCOM_cold),
               values_to = "Temp",
               names_to = "Model") %>%
  mutate(Model2 = case_when(
    Model == "EFDC_temp" ~ "EFDC",
    Model == "FVCOM_cold" ~ "FVCOM April start",
    Model == "FVCOM_hot" ~ "FVCOM Jan start"
  ))

colorvar <- c("green", "blue", "red")

ggplot(mvm_temp2, aes(x = date_time, y = Temp, color = Model2)) + 
  geom_point(alpha = 0.5) + scale_color_manual(values = colorvar) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%b") + 
  labs(x = "Date", y = "Temperature °C") + theme_classic()

ggsave("plots/efdc_vs_fvcom-hot_cold.png")

################################################################################
# Statistics

#### Calculate statistics - whole lake
realtemp <- mvm_temp$Real_temp
predictedEFDC <- mvm_temp$EFDC_temp
predictedCold <- mvm_temp$FVCOM_cold
predictedHot <- mvm_temp$FVCOM_hot

#RMSE - rmse(actual, predicted)
RMSE_temp <- c(
  RMSE_EFDC <- rmse(realtemp, predictedEFDC),
  RMSE_FVCOM_cold <- rmse(realtemp, predictedCold),
  RMSE_FVCOM_hot <- rmse(realtemp, predictedHot))

#MAE - mae(actual, predicted)
MAE_temp <- c(
  MAE_EFDC <- mae(realtemp, predictedEFDC),
  MAE_FVCOM_cold <- mae(realtemp, predictedCold),
  MAE_FVCOM_hot <- mae(realtemp, predictedHot))

#BIAS - bias(actual, predicted)
BIAS_temp <- c(
  BIAS_EFDC <- bias(realtemp, predictedEFDC),
  BIAS_FVCOM_cold <- bias(realtemp, predictedCold),
  BIAS_FVCOM_hot <- bias(realtemp, predictedHot))

#SKILL 2018 skill(predicted, actual)
SKILL_temp <- c(
  SKILL_EFDC <- skill.stat(predictedEFDC, realtemp),
  SKILL_FVCOM_cold <- skill.stat(predictedCold, realtemp),
  SKILL_FVCOM_hot <- skill.stat(predictedHot, realtemp))

#### very nearshore (< 5m)
realtemp_5m <- mvm_temp$Real_temp[mvm_temp$siteDepth < 5]
predictedEFDC_5m <- mvm_temp$EFDC_temp[mvm_temp$siteDepth < 5]
predictedFVCOM_cold_5m <- mvm_temp$FVCOM_cold[mvm_temp$siteDepth < 5]
predictedFVCOM_hot_5m <- mvm_temp$FVCOM_hot[mvm_temp$siteDepth < 5]

#RMSE - rmse(actual, predicted)
RMSE_temp_5m <- c(
  RMSE_EFDC_5m <- rmse(realtemp_5m, predictedEFDC_5m),
  RMSE_FVCOM_cold_5m <- rmse(realtemp_5m, predictedFVCOM_cold_5m),
  RMSE_FVCOM_hot_5m <- rmse(realtemp_5m, predictedFVCOM_hot_5m)
  )

#MAE - mae(actual, predicted)
MAE_temp_5m <- c(
  MAE_EFDC_5m <- mae(realtemp_5m, predictedEFDC_5m),
  MAE_FVCOM_cold_5m <- mae(realtemp_5m, predictedFVCOM_cold_5m),
  MAE_FVCOM_hot_5m <- mae(realtemp_5m, predictedFVCOM_hot_5m)
  )

#BIAS - bias(actual, predicted)
BIAS_temp_5m <- c(
  BIAS_EFDC_5m <- bias(realtemp_5m, predictedEFDC_5m),
  BIAS_FVCOM_cold_5m <- bias(realtemp_5m, predictedFVCOM_cold_5m),
  BIAS_FVCOM_hot_5m <- bias(realtemp_5m, predictedFVCOM_hot_5m)
  )

#SKILL 2018 skill(predicted, actual)
SKILL_temp_5m <- c(
  SKILL_EFDC_5m <- skill.stat(predictedEFDC_5m, realtemp_5m),
  SKILL_FVCOM_cold_5m <- skill.stat(predictedFVCOM_cold_5m, realtemp_5m),
  SKILL_FVCOM_hot_5m <- skill.stat(predictedFVCOM_hot_5m, realtemp_5m))

### nearshore (5 - 30m)
realtemp_5_30m <- mvm_temp$Real_temp[mvm_temp$siteDepth > 5 & 
                                       mvm_temp$siteDepth < 30]
predictedEFDC_5_30m <- mvm_temp$EFDC_temp[mvm_temp$siteDepth > 5 &
                                            mvm_temp$siteDepth < 30]
predictedFVCOM_cold_5_30m <- mvm_temp$FVCOM_cold[mvm_temp$siteDepth > 5 &
                                                   mvm_temp$siteDepth < 30]
predictedFVCOM_hot_5_30m <- mvm_temp$FVCOM_hot[mvm_temp$siteDepth > 5 &
                                                   mvm_temp$siteDepth < 30]

#RMSE - rmse(actual, predicted)
RMSE_temp_5_30m <- c(
  RMSE_EFDC_5_30m <- rmse(realtemp_5_30m, predictedEFDC_5_30m),
  RMSE_FVCOM_cold_5_30m <- rmse(realtemp_5_30m, predictedFVCOM_cold_5_30m),
  RMSE_FVCOM_hot_5_30m <- rmse(realtemp_5_30m, predictedFVCOM_hot_5_30m))

#MAE - mae(actual, predicted)
MAE_temp_5_30m <- c(
  MAE_EFDC_5_30m <- mae(realtemp_5_30m, predictedEFDC_5_30m),
  MAE_FVCOM_cold_5_30m <- mae(realtemp_5_30m, predictedFVCOM_cold_5_30m),
  MAE_FVCOM_hot_5_30m <- mae(realtemp_5_30m, predictedFVCOM_hot_5_30m))

#BIAS - bias(actual, predicted)
BIAS_temp_5_30m <- c(
  BIAS_EFDC_5_30m <- bias(realtemp_5_30m, predictedEFDC_5_30m),
  BIAS_FVCOM_cold_5_30m <- bias(realtemp_5_30m, predictedFVCOM_cold_5_30m),
  BIAS_FVCOM_hot_5_30m <- bias(realtemp_5_30m, predictedFVCOM_hot_5_30m))

#SKILL 2018 skill(predicted, actual)
SKILL_temp_5_30m <- c(
  SKILL_EFDC_5_30m <- skill.stat(predictedEFDC_5_30m, realtemp_5_30m),
  SKILL_FVCOM_cold_5_30m <- skill.stat(predictedFVCOM_cold_5_30m, realtemp_5_30m),
  SKILL_FVCOM_hot_5_30m <- skill.stat(predictedFVCOM_hot_5_30m, realtemp_5_30m))

#### Offshore (> 30m)
realtemp_30m <- mvm_temp$Real_temp[mvm_temp$siteDepth > 30]
predictedEFDC_30m <- mvm_temp$EFDC_temp[mvm_temp$siteDepth > 30]
predictedFVCOM_cold_30m <- mvm_temp$FVCOM_cold[mvm_temp$siteDepth > 30]
predictedFVCOM_hot_30m <- mvm_temp$FVCOM_hot[mvm_temp$siteDepth > 30]

#RMSE - rmse(actual, predicted)
RMSE_temp_30m <- c(
  RMSE_EFDC_30m <- rmse(realtemp_30m, predictedEFDC_30m),
  RMSE_FVCOM_cold_30m <- rmse(realtemp_30m, predictedFVCOM_cold_30m),
  RMSE_FVCOM_hot_30m <- rmse(realtemp_30m, predictedFVCOM_hot_30m)
  )

#MAE - mae(actual, predicted)
MAE_temp_30m <- c(
  MAE_EFDC_30m <- mae(realtemp_30m, predictedEFDC_30m),
  MAE_FVCOM_cold_30m <- mae(realtemp_30m, predictedFVCOM_cold_30m),
  MAE_FVCOM_hot_30m <- mae(realtemp_30m, predictedFVCOM_hot_30m)
  )

#BIAS - bias(actual, predicted)
BIAS_temp_30m <- c(
  BIAS_EFDC_30m <- bias(realtemp_30m, predictedEFDC_30m),
  BIAS_FVCOM_cold_30m <- bias(realtemp_30m, predictedFVCOM_cold_30m),
  BIAS_FVCOM_hot_30m <- bias(realtemp_30m, predictedFVCOM_hot_30m)
  )

#SKILL 2018 skill(predicted, actual)
SKILL_temp_30m <- c(
  SKILL_EFDC_30m <- skill.stat(predictedEFDC_30m, realtemp_30m),
  SKILL_FVCOM_cold_30m <- skill.stat(predictedFVCOM_cold_30m, realtemp_30m),
  SKILL_FVCOM_hot_30m <- skill.stat(predictedFVCOM_hot_30m, realtemp_30m)
  )
 
#Putting the individual statistic dataframes together into one dataframe 
Model <- c("EFDC", "FVCOM cold", "FVCOM hot")
tempstats <- data.frame(Model, RMSE_temp, MAE_temp, BIAS_temp, SKILL_temp,
                        RMSE_temp_5m, MAE_temp_5m, BIAS_temp_5m, SKILL_temp_5m,
                        RMSE_temp_5_30m, MAE_temp_5_30m, BIAS_temp_5_30m, 
                        SKILL_temp_5_30m, RMSE_temp_30m, MAE_temp_30m, BIAS_temp_30m,
                        SKILL_temp_30m)
tempstats

write.csv(tempstats, "Statistics/temp/temp_stats_v2.csv")

###############################################################################
# analysis for early vs late
mvm_temp <- mvm_temp %>%
  mutate(season = case_when(
    date_time <= "2018-05-31" ~ "early",
    date_time <= "2018-07-31" ~ "mid",
    date_time <= "2018-10-01" ~ "late"
  ))

#### early sim
realtemp_early <- mvm_temp$Real_temp[mvm_temp$season == "early"]
predictedEFDC_early <- mvm_temp$EFDC_temp[mvm_temp$season == "early"]
predictedFVCOM_cold_early <- mvm_temp$FVCOM_cold[mvm_temp$season == "early"]
predictedFVCOM_hot_early <- mvm_temp$FVCOM_hot[mvm_temp$season == "early"]

#RMSE - rmse(actual, predicted)
RMSE_temp_early <- c(
  RMSE_EFDC_early <- rmse(realtemp_early, predictedEFDC_early),
  RMSE_FVCOM_cold_early <- rmse(realtemp_early, predictedFVCOM_cold_early),
  RMSE_FVCOM_hot_early <- rmse(realtemp_early, predictedFVCOM_hot_early)
)

#MAE - mae(actual, predicted)
MAE_temp_early <- c(
  MAE_EFDC_early <- mae(realtemp_early, predictedEFDC_early),
  MAE_FVCOM_cold_early <- mae(realtemp_early, predictedFVCOM_cold_early),
  MAE_FVCOM_hot_early <- mae(realtemp_early, predictedFVCOM_hot_early)
)

#BIAS - bias(actual, predicted)
BIAS_temp_early <- c(
  BIAS_EFDC_early <- bias(realtemp_early, predictedEFDC_early),
  BIAS_FVCOM_cold_early <- bias(realtemp_early, predictedFVCOM_cold_early),
  BIAS_FVCOM_hot_early <- bias(realtemp_early, predictedFVCOM_hot_early)
)

#SKILL 2018 skill(predicted, actual)
SKILL_temp_early <- c(
  SKILL_EFDC_early <- skill.stat(predictedEFDC_early, realtemp_early),
  SKILL_FVCOM_cold_early <- skill.stat(predictedFVCOM_cold_early, realtemp_early),
  SKILL_FVCOM_hot_early <- skill.stat(predictedFVCOM_hot_early, realtemp_early)
)
#### mid sim
realtemp_mid <- mvm_temp$Real_temp[mvm_temp$season == "mid"]
predictedEFDC_mid <- mvm_temp$EFDC_temp[mvm_temp$season == "mid"]
predictedFVCOM_cold_mid <- mvm_temp$FVCOM_cold[mvm_temp$season == "mid"]
predictedFVCOM_hot_mid <- mvm_temp$FVCOM_hot[mvm_temp$season == "mid"]

#RMSE - rmse(actual, predicted)
RMSE_temp_mid <- c(
  RMSE_EFDC_mid <- rmse(realtemp_mid, predictedEFDC_mid),
  RMSE_FVCOM_cold_mid <- rmse(realtemp_mid, predictedFVCOM_cold_mid),
  RMSE_FVCOM_hot_mid <- rmse(realtemp_mid, predictedFVCOM_hot_mid)
)

#MAE - mae(actual, predicted)
MAE_temp_mid <- c(
  MAE_EFDC_mid <- mae(realtemp_mid, predictedEFDC_mid),
  MAE_FVCOM_cold_mid <- mae(realtemp_mid, predictedFVCOM_cold_mid),
  MAE_FVCOM_hot_mid <- mae(realtemp_mid, predictedFVCOM_hot_mid)
)

#BIAS - bias(actual, predicted)
BIAS_temp_mid <- c(
  BIAS_EFDC_mid <- bias(realtemp_mid, predictedEFDC_mid),
  BIAS_FVCOM_cold_mid <- bias(realtemp_mid, predictedFVCOM_cold_mid),
  BIAS_FVCOM_hot_mid <- bias(realtemp_mid, predictedFVCOM_hot_mid)
)

#SKILL 2018 skill(predicted, actual)
SKILL_temp_mid <- c(
  SKILL_EFDC_mid <- skill.stat(predictedEFDC_mid, realtemp_mid),
  SKILL_FVCOM_cold_mid <- skill.stat(predictedFVCOM_cold_mid, realtemp_mid),
  SKILL_FVCOM_hot_mid <- skill.stat(predictedFVCOM_hot_mid, realtemp_mid)
  )

#### late sim
realtemp_late <- mvm_temp$Real_temp[mvm_temp$season == "late"]
predictedEFDC_late <- mvm_temp$EFDC_temp[mvm_temp$season == "late"]
predictedFVCOM_cold_late <- mvm_temp$FVCOM_cold[mvm_temp$season == "late"]
predictedFVCOM_hot_late <- mvm_temp$FVCOM_hot[mvm_temp$season == "late"]

#RMSE - rmse(actual, predicted)
RMSE_temp_late <- c(
  RMSE_EFDC_late <- rmse(realtemp_late, predictedEFDC_late),
  RMSE_FVCOM_cold_late <- rmse(realtemp_late, predictedFVCOM_cold_late),
  RMSE_FVCOM_hot_late <- rmse(realtemp_late, predictedFVCOM_hot_late)
)

#MAE - mae(actual, predicted)
MAE_temp_late <- c(
  MAE_EFDC_late <- mae(realtemp_late, predictedEFDC_late),
  MAE_FVCOM_cold_late <- mae(realtemp_late, predictedFVCOM_cold_late),
  MAE_FVCOM_hot_late <- mae(realtemp_late, predictedFVCOM_hot_late)
)

#BIAS - bias(actual, predicted)
BIAS_temp_late <- c(
  BIAS_EFDC_late <- bias(realtemp_late, predictedEFDC_late),
  BIAS_FVCOM_cold_late <- bias(realtemp_late, predictedFVCOM_cold_late),
  BIAS_FVCOM_hot_late <- bias(realtemp_late, predictedFVCOM_hot_late)
)

#SKILL 2018 skill(predicted, actual)
SKILL_temp_late <- c(
  SKILL_EFDC_late <- skill.stat(predictedEFDC_late, realtemp_late),
  SKILL_FVCOM_cold_late <- skill.stat(predictedFVCOM_cold_late, realtemp_late),
  SKILL_FVCOM_hot_late <- skill.stat(predictedFVCOM_hot_late, realtemp_late)
)

#Putting the individual statistic dataframes together into one dataframe 
Model <- c("EFDC", "FVCOM cold", "FVCOM hot")
tempstats2 <- data.frame(Model, RMSE_temp, MAE_temp, BIAS_temp, SKILL_temp,
                        RMSE_temp_early, MAE_temp_early, BIAS_temp_early, SKILL_temp_early,
                        RMSE_temp_mid, MAE_temp_mid, BIAS_temp_mid, SKILL_temp_mid, 
                        RMSE_temp_late, MAE_temp_late, BIAS_temp_late, SKILL_temp_late)
tempstats2

write.csv(tempstats, "Statistics/temp/temp_stats_season.csv")
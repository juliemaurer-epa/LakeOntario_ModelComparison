# Modeled vs Measured TP 
# Script to run in background to generate MvM tables (input for Rstats_Lake_Ontario.R)

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/")

# packages
library(lubridate)
library(ncdf4)
library(dplyr) 
library(ggplot2)

# functions
source("../scripts/model2data/nodematchFVCOM.R")
source("../scripts/model2data/cellmatchEFDC.R")

########################## LOAD OBSERVATIONAL DATA ###############################
# TP data with Hamilton Harbor points removed
df <- read.csv("/work/GLHABS/GreatLakesEco/LakeOntario/TPData_processing/TP_observationaldata_combined_noHH_efdc-fvcom.csv", 
               header = TRUE) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "GMT", tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
         JulianDate = as.numeric(date_time),
         Year = factor(Year, levels = c("2013", "2018"))) %>%
  select(-c(X.3, X.2, X.1, X))

# FVCOM sigma: converting 0s -> 1 and >20 -> 20
df$sigma <- replace(df$sigma, df$sigma < 1, 1)
df$sigma <- replace(df$sigma, df$sigma > 20, 20)

# K_Index: converting 0s -> 1 and >10 -> 10
df$K_Index <- replace(df$K_Index, df$K_Index < 1, 1)
df$K_Index <- replace(df$K_Index, df$K_Index > 10, 10)

#save updates
write.csv("/work/GLHABS/GreatLakesEco/LakeOntario/TPData_processing/TP_observationaldata_combined_noHH_efdc-fvcom.csv")

########################### FVCOM TP Scenario 1 ################################
output_folder <- "/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/scenario1/"
factor_ug <- 1e3 # converting mg/L -> ug/L (FVCOM TP & LOEM)

## Input netcdf
fvcom <- "/work/GLHABS/wmelende/FVCOM_LO_Test/scenario1/output/fvcom_0001.nc"

nc <- nc_open(fvcom)
time <- ncvar_get(nc, varid = "time") #seconds since 01-01-1970 for EFDC-TP & LOEM, days since 01-01-1970 for FVCOM-TP
time <- time*(24*60*60) # convert to seconds

param <- ncvar_get(nc, varid = "TP") #TR for EFDC-TP, TP for LOEM & FVCOM-TP

maxTime <- max(time)
minTime <- min(time)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minTime,]
df <- df[df$JulianDate <= maxTime,]

##  Generate modeled vs measured Temp dataframe

# calculating nearest model value to measured values with function
modelvalue <- nodematchFVCOM(param, time, df)

modeled_value <- modelvalue * factor_ug

dfOut <- df
dfOut$modelvalue <- modeled_value
write.csv(dfOut, 
          file = file.path(output_folder, "Modeled-v-Measured_TP_2018_FVCOM_scenario1.csv", fsep = "/"), 
          row.names = FALSE) 

########################### FVCOM TP Scenario 2 ################################
output_folder <- "/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/scenario2/"
factor_ug <- 1e3 # converting mg/L -> ug/L (FVCOM TP & LOEM)

## Input netcdf
fvcom <- "/work/GLHABS/wmelende/FVCOM_LO_Test/scenario2/output/fvcom_0001.nc"

nc <- nc_open(fvcom)
time <- ncvar_get(nc, varid = "time") #seconds since 01-01-1970 for EFDC-TP & LOEM, days since 01-01-1970 for FVCOM-TP
time <- time*(24*60*60) #convert to seconds

param <- ncvar_get(nc, varid = "TP") #TR for EFDC-TP, TP for LOEM & FVCOM-TP

maxTime <- max(time)
minTime <- min(time)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minTime,]
df <- df[df$JulianDate <= maxTime,]

##  Generate modeled vs measured Temp dataframe

# calculating nearest model value to measured values with function
modelvalue <- nodematchFVCOM(param, time, df)

modeled_value <- modelvalue * factor_ug

dfOut <- df
dfOut$modelvalue <- modeled_value
write.csv(dfOut, 
          file = file.path(output_folder, "Modeled-v-Measured_TP_2018_FVCOM_scenario2.csv", fsep = "/"), 
          row.names = FALSE) 

############################ EFDC TP Scenario 1 ################################
output_folder <- "/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/scenario1/"
factor_ug <- 1e6 # converting kg/m3 -> ug/L (EFDC TP)

## Input netcdf
InputFile <- "/work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO_scenario1/NETCDF/gomdom.000000.nc"

nc <- nc_open(InputFile)
time <- ncvar_get(nc, varid = "time") #seconds since 01-01-1970 for EFDC-TP & LOEM, days since 01-01-1970 for FVCOM-TP

param <- ncvar_get(nc, varid = "TR") #TR for EFDC-TP, TP for LOEM & FVCOM-TP

maxDate <- max(time)
minDate <- min(time)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minDate,]
df <- df[df$JulianDate <= maxDate,]

##  Generate modeled vs measured Temp dataframe

# calculating nearest model value to measured values with function
modelvalue <- cellmatchEFDC(param, time, df)

modeled_value <- modelvalue * factor_ug

dfOut <- df
dfOut$modelvalue <- modeled_value
write.csv(dfOut, 
          file = file.path(output_folder, "Modeled-v-Measured_TP_2018_EFDC_scenario1.csv", fsep = "/"), 
          row.names = FALSE) 

############################ EFDC TP Scenario 2 ################################
output_folder <- "/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/scenario2/"
factor_ug <- 1e6 # converting kg/m3 -> ug/L (EFDC TP)

## Input netcdf
InputFile <- "/work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO_scenario2/NETCDF/gomdom.000000.nc"

nc <- nc_open(InputFile)
time <- ncvar_get(nc, varid = "time") #seconds since 01-01-1970 for EFDC-TP & LOEM, days since 01-01-1970 for FVCOM-TP

param <- ncvar_get(nc, varid = "TR") #TR for EFDC-TP, TP for LOEM & FVCOM-TP

maxDate <- max(time)
minDate <- min(time)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minDate,]
df <- df[df$JulianDate <= maxDate,]

##  Generate modeled vs measured Temp dataframe

# calculating nearest model value to measured values with function
modelvalue <- cellmatchEFDC(param, time, df)

measured_value <- df$temp 
modeled_value <- modelvalue * factor_ug

dfOut <- df
dfOut$modelvalue <- modeled_value
write.csv(dfOut, 
          file = file.path(output_folder, "Modeled-v-Measured_TP_2018_EFDC_scenario2.csv", fsep = "/"), 
          row.names = FALSE) 

############################ EFDC TP Scenario 3 ################################
output_folder <- "/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/scenario3/"
factor_ug <- 1e6 # converting kg/m3 -> ug/L (EFDC TP)

## Input netcdf
InputFile <- "/work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO_scenario3/NETCDF/gomdom.000000.nc"

nc <- nc_open(InputFile)
time <- ncvar_get(nc, varid = "time") #seconds since 01-01-1970 for EFDC-TP & LOEM, days since 01-01-1970 for FVCOM-TP

param <- ncvar_get(nc, varid = "TR") #TR for EFDC-TP, TP for LOEM & FVCOM-TP

maxDate <- max(time)
minDate <- min(time)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minDate,]
df <- df[df$JulianDate <= maxDate,]

##  Generate modeled vs measured Temp dataframe

# calculating nearest model value to measured values with function
modelvalue <- cellmatchEFDC(param, time, df)

measured_value <- df$temp 
modeled_value <- modelvalue * factor_ug

dfOut <- df
dfOut$modelvalue <- modeled_value
write.csv(dfOut, 
          file = file.path(output_folder, "Modeled-v-Measured_TP_2018_EFDC_scenario3.csv", fsep = "/"), 
          row.names = FALSE) 

############################# LOEM Scenario 1 ##################################
output_folder <- "/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/scenario1/"
factor_ug <- 1e3 # converting mg/L -> ug/L 

## Input netcdf
InputFile <- "/work/GLFBREEZ/LOEM/LOEM_Winmodel_nc/Scenario1/LOEM_2018_scenario1.nc"

nc <- nc_open(InputFile)
time <- ncvar_get(nc, varid = "datetime") #seconds since 01-01-1970 for EFDC-TP & LOEM, days since 01-01-1970 for FVCOM-TP

param <- ncvar_get(nc, varid = "TP") #TR for EFDC-TP, TP for LOEM & FVCOM-TP

maxDate <- max(time)
minDate <- min(time)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minDate,]
df <- df[df$JulianDate <= maxDate,]

##  Generate modeled vs measured Temp dataframe

# calculating nearest model value to measured values with function
modelvalue <- cellmatchEFDC(param, time, df)

measured_value <- df$temp 
modeled_value <- modelvalue * factor_ug

dfOut <- df
dfOut$modelvalue <- modeled_value
write.csv(dfOut, 
          file = file.path(output_folder, "Modeled-v-Measured_TP_2018_LOEM_scenario1.csv", fsep = "/"), 
          row.names = FALSE) 

############################# LOEM Scenario 2 ##################################
output_folder <- "/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/scenario2/"
factor_ug <- 1e3 # converting mg/L -> ug/L 

## Input netcdf
InputFile <- "/work/GLFBREEZ/LOEM/LOEM_Winmodel_nc/Scenario2/LOEM_2018_scenario2.nc"

nc <- nc_open(InputFile)
time <- ncvar_get(nc, varid = "datetime") #seconds since 01-01-1970 for EFDC-TP & LOEM, days since 01-01-1970 for FVCOM-TP

param <- ncvar_get(nc, varid = "TP") #TR for EFDC-TP, TP for LOEM & FVCOM-TP

maxDate <- max(time)
minDate <- min(time)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minDate,]
df <- df[df$JulianDate <= maxDate,]

##  Generate modeled vs measured Temp dataframe

# calculating nearest model value to measured values with function
modelvalue <- cellmatchEFDC(param, time, df)

measured_value <- df$temp 
modeled_value <- modelvalue * factor_ug

dfOut <- df
dfOut$modelvalue <- modeled_value
write.csv(dfOut, 
          file = file.path(output_folder, "Modeled-v-Measured_TP_2018_LOEM_scenario2.csv", fsep = "/"), 
          row.names = FALSE) 

############################# LOEM Scenario 3 ##################################
output_folder <- "/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/scenario3/"
factor_ug <- 1e3 # converting mg/L -> ug/L 

## Input netcdf
InputFile <- "/work/GLFBREEZ/LOEM/LOEM_Winmodel_nc/Scenario3/LOEM_2018_scenario3.nc"

nc <- nc_open(InputFile)
time <- ncvar_get(nc, varid = "datetime") #seconds since 01-01-1970 for EFDC-TP & LOEM, days since 01-01-1970 for FVCOM-TP

param <- ncvar_get(nc, varid = "TP") #TR for EFDC-TP, TP for LOEM & FVCOM-TP

maxDate <- max(time)
minDate <- min(time)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minDate,]
df <- df[df$JulianDate <= maxDate,]

##  Generate modeled vs measured Temp dataframe

# calculating nearest model value to measured values with function
modelvalue <- cellmatchEFDC(param, time, df)

measured_value <- df$temp 
modeled_value <- modelvalue * factor_ug

dfOut <- df
dfOut$modelvalue <- modeled_value
write.csv(dfOut, 
          file = file.path(output_folder, "Modeled-v-Measured_TP_2018_LOEM_scenario3.csv", fsep = "/"), 
          row.names = FALSE) 

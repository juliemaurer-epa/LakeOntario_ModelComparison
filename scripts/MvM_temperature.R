#[] find/replace modeled_t with Modeled_temp and measured_t with Measured_temp

# Modeled vs Measured Temperature: FVCOM
# Author: Julie Maurer
# Created: 10/15/25

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/")

library(lubridate)
library(ncdf4)
library(dplyr) 
library(ggplot2)
source("../scripts/nodematchFVCOM.R")
source("../scripts/cellmatchEFDC.R")

# change the following for different scenarios/models/parameters
output_folder <- "Statistics/temp/"

#### Input data and netcdfs ####

# station data
df <- read.csv("../TPData_processing/Temp_observationaldata_combined_loem-fvcom.csv", 
                 header = TRUE)
df$date_time <- as.Date(df$date_time, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
df$JulianDate <- as.numeric(df$date_time) 

# K_Index: converting 0s -> 1 and >10 -> 10
df$K_Index <- replace(df$K_Index, df$K_Index < 1, 1)
df$K_Index <- replace(df$K_Index, df$K_Index > 10, 10)

#################################################################################
# FVCOM
fvcom <- "/work/GLHABS/wmelende/FVCOM_LO_Test/scenario1/output/fvcom_0001.nc"

nc <- nc_open(fvcom)
time <- ncvar_get(nc, varid = "time") #days since 1970-01-01

param <- ncvar_get(nc, varid = "temp") 

maxTime <- max(time)
minTime <- min(time)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minTime,]
df <- df[df$JulianDate <= maxTime,]

#  Generate modeled vs measured Temp dataframe

# calculating nearest model value to measured values with function
modelvalue <- nodematchFVCOM(param, time, df)

measured_value <- df$temp 

dfOut <- df
dfOut$modelvalue <- modelvalue
write.csv(dfOut, 
          file = file.path(output_folder, "Modeled-v-Measured_Temp_2018_FVCOM.csv", fsep = "/"), 
          row.names = FALSE) 

#################################################################################
# EFDC
efdc <- "/work/GLFBREEZ/Lake_Ontario/CGEM/CGEM/data/Lake_Ontario_2018/INPUT/Temp.nc"

nc <- nc_open(efdc)
stime <- ncvar_get(nc, varid = "Time") #seconds since 1970-01-01
time <- stime/(60*60*24) #convert to days

param <- ncvar_get(nc, varid = "Temp") 

maxTime <- max(time)
minTime <- min(time)

nc_close(nc)

# Only use field data within model run time period
df <- df[df$JulianDate >= minTime,]
df <- df[df$JulianDate <= maxTime,]

#  Generate modeled vs measured Temp dataframe

# calculating nearest model value to measured values with function
modelvalue <- cellmatchEFDC(param, time, df)

measured_value <- df$temp 

dfOut <- df
dfOut$modelvalue <- modelvalue
write.csv(dfOut, 
          file = file.path(output_folder, "Modeled-v-Measured_Temp_2018_EFDC.csv", fsep = "/"), 
          row.names = FALSE) 

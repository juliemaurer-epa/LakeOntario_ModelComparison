## nodematchFVCOM
# Function: nodematchFVCOM
# Purpose: for matching fvcom nodes for water quality variable and time to observational data
# Author: Julie Maurer
# Created: 10/15/2025

#INPUTS: 
# param = wq parameter from model netcdf
# stime = time parameter from model netcdf
# df = observational dataframe - column names MUST match

nodematchFVCOM <- function(param, stime, df) {
  
  nParam <- nrow(df)
  
  modelvalue <- c(1:nParam)
  
  for (i in 1:nParam) {
    # find node of closest model output date to each field date
    date_index <- which.min(abs(df$JulianDate[i] - stime))
    
    # find modeled value closest to field data time and location
    modelvalue[i] <- param[df$node_fvcom[i], df$sigma[i], date_index]
  }
  
  return(modelvalue)
}


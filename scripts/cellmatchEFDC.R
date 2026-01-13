## cellmatchEFDC
# Function: cellmatchEFDC
# Purpose: for matching efdc cells for water quality variable and time to observational data
# Author: Julie Maurer
# Created: 10/15/2025

#INPUTS: 
# param = wq parameter from model netcdf
# time = time parameter from model netcdf
# df = observational dataframe - column names MUST match

cellmatchEFDC <- function(param, time, df) {
  
  nParam <- nrow(df)
  
  modelvalue <- c(1:nParam)
  
  for (i in 1:nParam) {
    # find node of closest model output date to each field date
    date_index <- which.min(abs(df$JulianDate[i] - time))
    
    # find modeled value closest to field data time and location
    modelvalue[i] <- param[df$I_Index[i], df$J_Index[i], df$K_Index[i], date_index]
  }
  
  return(modelvalue)
}
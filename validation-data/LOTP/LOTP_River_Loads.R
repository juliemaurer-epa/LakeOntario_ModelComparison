## This script is for updating river input netcdf files to run LOTP model under various loading scenarios.

library(ncdf4)
library(openxlsx)

# Set name of Excel file containing river loads.
TP_InputFile <- "LOEM_TP_Daily_Loads.xlsx"

# Set name of netcdf containing TP loads.
Netcdf_InputFile <- "TP_RiverLoads_2013.nc"

# Read in Excel file containing river loads.
# The command below assumes user is reading input data from an Excel file.  
# The "sheet" parameter should be adjusted to whatever name the Excel file 
# has for the sheet containing the data.
# If using a CSV file, use the "read.csv" function instead.
dfTP <- read.xlsx(xlsxFile = TP_InputFile,
                  sheet = "TP_Loads_g_s",
                  colNames = TRUE,
                  detectDates = TRUE)

# Create data frame for Niagara River (or some other river).  
# Data frame is defined for 365 days.  
# If updating loads for leap year, set length.out to 366.
# Set date to appropriate year. Current example assumes year is 2013.
# Note: If updating additional rivers, add more columns to data frame below.
dfLD <- data.frame(Date = seq(as.Date("2013/1/1"), by = "day", length.out = 365),
                   Load1 = 0.0)

# Obtain the day numbers for the simulation time frame.
dayNumbers <- match(dfTP$Date, dfLD$Date)

# Assign Niagara River loads to the corresponding day numbers in data frame.
dfLD$Load1[dayNumbers] <- dfTP$Niagara

# Repeat the April 1st TP load for the January 1 - March 31 time frame.
date1 <- match("2013-01-01", dfLD$Date)
date2 <- match("2013-03-31", dfLD$Date)
date3 <- match("2013-04-01", dfLD$Date)
dfLD$Load1[date1:date2] <- dfLD$Load1[date3]

# Repeat the September 30th TP load for the October 1 - December 31 time frame.
date4 <- match("2013-10-01", dfLD$Date)
date5 <- match("2013-12-31", dfLD$Date)
date6 <- match("2013-09-30", dfLD$Date)
dfLD$Load1[date4:date5] <- dfLD$Load1[date6]
  
# Open netcdf file.
nc <- nc_open(Netcdf_InputFile, write = TRUE)

# Read in TP variable from netcdf file.
tp <- ncvar_get(nc, varid = "TP")

# Update Niagara River. Note Niagara River corresponds to index 1.
# If updating more rivers, add corresponding entries below.
tp[1,] <- dfLD$Load1

# Put updated values under TP variable in netcdf file.
ncvar_put(nc, varid = "TP", vals = tp)

# Close netcdf file.
nc_close(nc)

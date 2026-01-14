library(ncdf4)
library(ggplot2)

##### EFDC files ####
nfile1 <-"/work/GLFBREEZ/Lake_Ontario/CGEM/CGEM/data/Lake_Ontario_2018/INPUT/Temp.nc"
nfile2 <- "/work/GLFBREEZ/Lake_Ontario/CGEM/CGEM/data/Lake_Ontario_2018/INPUT/SurfaceElev.nc"
nfile3 <- "/work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO_scenario1/NETCDF/gomdom.000000.nc"

nx <- 256
ny <- 133

nc1 <- nc_open(nfile1)
nc2 <- nc_open(nfile2)
nc3 <- nc_open(nfile3)

# Read in water temperature
temp <- ncvar_get(nc1, varid = "Temp")

tdates <- ncvar_get(nc1, varid = "Time")

nt <- length(tdates)

tdates1 <- as.POSIXct(tdates, tz = "GMT")
# tdates1 <- gsub("GMT", "", tdates1)

# Read in sea surface elevations
sse <- ncvar_get(nc2, varid = "SurfaceElev")

# Read in still-water depths
h <- ncvar_get(nc3, varid = "h")

# Read in areas
area <- ncvar_get(nc3, varid = "Area")

# Read in cells' thicknesses
dz <- ncvar_get(nc3, varid = "dz")

# Read in land/water mask array
fm <- ncvar_get(nc3, varid = "fm")

# Close netcdf files
nc_close(nc1)
nc_close(nc2)
nc_close(nc3)

# Calculate grid cell thicknesses as a function of time
dz1 <- array(0.0, dim = c(nx,ny,nt))
for (t in 1:nt)
{
  for (i in 1:nx)
  {
    for (j in 1:ny)
    {
      if (fm[i,j,1] == 1)
      {
        nL <- max(which(h[i,j,] > 0))
        dz1[i,j,t] <- dz[i,j,1] + sse[i,j,t]/nL
      }
    }
  }
}

# Calculate surface cell volumes
vol <- array(0.0, dim = c(nx,ny,nt))
for (t in 1:nt)
{
  for (i in 1:nx)
  {
    for (j in 1:ny)
    {
      if (fm[i,j,1] == 1)
      {
        vol[i,j,t] <- area[i,j] * dz1[i,j,t]
      }
    }
  }
}

# Calculate volume-weighted surface temperatures
vol_weighted_temp <- array(0.0, dim = c(nt))
for (t in 1:nt)
{
  sum_vol_temp <- 0.0
  sum_vol <- 0.0
  for (i in 1:nx)
  {
    for (j in 1:ny)
    {
      if (fm[i,j,1] == 1)
      {
        sum_vol_temp <- sum_vol_temp + (temp[i,j,1,t] * vol[i,j,t])
        sum_vol <- sum_vol + vol[i,j,t]
      }
    }
  }
  vol_weighted_temp[t] <- sum_vol_temp/sum_vol
}

# Define data frame
df <- data.frame(Date = tdates1,
                 Temperature = vol_weighted_temp, 
                 Model = "EFDC") 

# Write date to a CSV file.
write.csv(df, file = "EFDC_VolWeighted_Temp.csv", row.names = FALSE)

##### FVCOM ####
nfile <- "/work/GLHABS/wmelende/FVCOM_LO_Test/scenario1/output/fvcom_coord.nc"

nodes <- 34395

# Open netcdf file
nc <- nc_open(nfile)

# Read in water temperature
temp <- ncvar_get(nc, varid = "temp")

# Read in the still-water depths
h <- ncvar_get(nc, varid = "h")

# Read in the sea surface elevations
zeta <- ncvar_get(nc, varid = "zeta")

# Read in the areas associated with the nodes
area <- ncvar_get(nc, varid = "art1")

# Read in the dates
tdates <- ncvar_get(nc, varid = "Times")

# Replace unwanted characters from dates
tdates <- gsub("T", " ", tdates)
tdates <- gsub(":00.000000", "", tdates)

# Convert tdate to POSIXct objects
tdates1 <- as.POSIXct(tdates)

# Obtain number of time records
trecs <- length(tdates1)

# Close netcdf file
nc_close(nc)

# Calculate water depths as a function of time
wd <- array(0.0, dim = c(nodes,trecs))
for (i in 1:trecs)
{
  wd[,i] <- h + zeta[,i]
}


# Calculate thicknesses of layers
ldz <- wd/20

# Calculate the volumes for the surface layers
vol <- array(0.0, dim = c(nodes,trecs))
for (i in 1:trecs)
{
  vol[,i] <- area * ldz[,i]
}

# Calculate average volume-weighted temperature for surface layer (k = 1)
vol_weighted_temp <- array(0.0, dim = c(trecs))
for (t in 1:trecs)
{
  sum_vol_temp <- 0.0
  for (inode in 1:nodes)
  {
    sum_vol_temp <- sum_vol_temp + (temp[inode,1,t] * vol[inode,t])
  }
  vol_weighted_temp[t] <- sum_vol_temp/sum(vol[,t])
}

# Define data frame
df2 <- data.frame(Date = tdates1,
                 Temperature = vol_weighted_temp,
                 Model = "FVCOM")

# Write date to a CSV file.
write.csv(df2, file = "FVCOM_VolWeighted_Temp.csv", row.names = FALSE)

##### figure ####
# add obs data
df.obs <- readRDS("/work/GLHABS/GreatLakesEco/LakeOntario/TPData_processing/Temp_data_combined.RData") %>%
  mutate(date_time = as.POSIXct(date_time, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")))

#trim data to sim period
datemin <- min(df$Date)
datemax <- max(df$Date)

df.obs <- df.obs[df.obs$date_time >= datemin & df.obs$date_time <= datemax, ]

#trim to top 5 meters
df.obs <- df.obs[df.obs$sampleDepth >= 0 & df.obs$sampleDepth <= 5, ]

colvar <- c("#56B4E9", "#E69F00") #EFDC = blue, FVCOM = orange

#gg1 <- ggplot() + geom_point(data = df.obs, aes(x = date_time, y = temp), color = "black", alpha = 0.5) 

gg2 <- ggplot() + geom_line(data = df2, aes(x = Date, y = Temperature, color = Model), linewidth = 1.5)

gg3 <- gg2 + geom_line(data = df, aes(x = Date, y = Temperature, color = Model), linewidth = 1.5)

gg4 <- gg3 + scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme_classic() + scale_color_manual(values = colvar) +
  labs(x = "Date", y = "Volume-Weigthed Surface Temperature (C)")
gg4
ggsave(filename = "EFDC_FVCOM_SurfaceLayer_Temp.png", plot = gg4)



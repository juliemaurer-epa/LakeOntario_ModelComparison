## FVCOM vs EFDC temperature comparison
# Author: Julie Maurer
# Created: 10/9/25

library(ncdf4)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(patchwork)

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison")

# netcdf output
fvcom <- nc_open("/work/GLHABS/wmelende/FVCOM_LO_Test/scenario1/output/fvcom_0001.nc")
efdc <- nc_open("/work/GLFBREEZ/Lake_Ontario/CGEM/CGEM/data/Lake_Ontario_2018/INPUT/Temp.nc")

# station data
temp.data <- readRDS("../TPData_processing/Temp_data_combined.RData") %>%
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y")) %>%
  dplyr::select(-c(X, X.1)) 

datemin <- as.POSIXct("2018-04-01")
datemax <- as.POSIXct("2018-10-01")

temp.data <- temp.data[temp.data$date_time >= datemin & temp.data$date_time <= datemax,]

#### observations over time and depth ####
pobsC <- ggplot(data = temp.data, aes(x = date_time, y = temp)) + geom_point(alpha = 0.5) +
  labs(x = "Date", y = "Temperature °C") + theme_classic() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"))
pobsC

temp.data2 <- temp.data %>%
  mutate(season = case_when(
    date_time <= "2018-05-31" ~ "early",
    date_time <= "2018-07-31" ~ "mid",
    date_time <= "2018-10-01" ~ "late"),
  season = factor(season, levels = c("early", "mid", "late"))
  )

pobsD <- ggplot(data = temp.data2, aes(x = season)) + geom_bar(fill = "black") + 
  labs(x = "Simulation Period", y = "Number of Observations") + theme_classic() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"))
pobsD

ggplot(data = temp.data, aes(x = type)) + geom_bar(stat = "count") 
#2 buoy sites - use both
# lots of csmi 

ggplot(data = temp.data, aes(x = K_Index, fill = type)) + geom_bar(stat = "count") #lots of surface data! 

################################################################################
#### Part1: Buoy sites - temporal comparison ####
# Buoy site 45012: surface temp over time
temp.data[temp.data$siteID == "45012",] %>% head() #fvcom node = 21753, efdc node = 9157

B.45012 <- temp.data[temp.data$siteID == "45012",]

#model output
temp1 <- ncvar_get(fvcom, varid = "temp", start = c(21753, 1, 1), count = c(1, 1, 4393))

# Read in the dates
tdates1 <- ncvar_get(fvcom, varid = "Times")

# Replace unwanted characters from dates
tdates1 <- gsub("T", " ", tdates1)
tdates1 <- gsub(":00.000000", "", tdates1)

# Convert tdate to POSIXct objects
tdates1 <- as.POSIXct(tdates1)

temp2 <- ncvar_get(efdc, varid = "Temp", start = c(181, 91, 1, 1), count = c(1, 1, 1, 1464))
time2 <- ncvar_get(efdc, varid = "Time") %>% 
  as.POSIXct(tz = "GMT", origin = "1970-01-01") 

df.fvcom <- data_frame(DateTime = tdates1,
                       Temp = temp1,
                       Model = "FVCOM")
write.csv(df.fvcom, "fvcom_buoy_out.csv")

df.efdc <- data_frame(DateTime = time2,
                      Temp = temp2,
                      Model = "EFDC")
write.csv(df.efdc, "efdc_buoy_out.csv")

#plotting
colvar <- c("#56B4E9", "#E69F00")

p1 <- ggplot() + geom_point(aes(x = B.45012$date_time, y = B.45012$temp), size = 1, alpha = 0.5)

p2 <- p1 + geom_line(aes(x = df.fvcom$DateTime, y = df.fvcom$Temp, color = df.fvcom$Model), linewidth = 1)

p3 <- p2 + geom_line(aes(df.efdc$DateTime, y = df.efdc$Temp, color = df.efdc$Model)) + 
  theme_classic() + labs(title = "Buoy 45012, (43.621, -77.401)", x = "Time", y = "Temperature °C", color = "Model") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%d") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30)) +
  scale_color_manual(values = colvar) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 12, color = "black"),
        legend.position = "bottom")
p3
ggsave(plot = p3, filename = "plots/EFDC-FVCOM-Temp-Buoy45012.png")

pB <- p3 + theme(axis.title.y = element_blank(),
                 axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

################################################################################
#### Part2: Volume-weighted surfance temperature comparison ####
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
df.efdc.surf <- data.frame(Date = tdates1,
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
df.fvcom.surf <- data.frame(Date = tdates1,
                  Temperature = vol_weighted_temp,
                  Model = "FVCOM")

# Write date to a CSV file.
write.csv(df2, file = "FVCOM_VolWeighted_Temp.csv", row.names = FALSE)

##### figure ####

colvar <- c("#56B4E9", "#E69F00") #EFDC = blue, FVCOM = orange

#gg1 <- ggplot() + geom_point(data = df.obs, aes(x = date_time, y = temp), color = "black", alpha = 0.5) 

gg2 <- ggplot() + geom_line(data = df.fvcom.surf, aes(x = Date, y = Temperature, color = Model), linewidth = 1)

gg3 <- gg2 + geom_line(data = df.efdc.surf, aes(x = Date, y = Temperature, color = Model), linewidth = 1)

gg4 <- gg3 + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%d") + 
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0,30)) + 
  theme_classic() + scale_color_manual(values = colvar) +
  labs(x = "Time", y = "Temperature °C", title = "Volume-Weighted Surface Temperature") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        legend.position = "bottom")
gg4
ggsave(filename = "EFDC_FVCOM_SurfaceLayer_Temp.png", plot = gg4)

pA <- gg4 + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
pA
################################################################################
#Part 3: residuals vs measured temp

# input: modeled vs measured dataframe generated in MvM_temperature.R
mvm_temp <- read.csv("Statistics/temp/Modeled-v-Measured_Temp_2018_efdc-v-fvcom_formatted.csv") %>%
  mutate(date_time = as.Date(date_time, format = "%Y-%m-%d"))

# calc residuals
mvm_temp_res <- mvm_temp %>%
  mutate(res_efdc = EFDC_temp - Real_temp,
         res_fvcom = FVCOM_temp - Real_temp) %>%
  pivot_longer(cols = c(res_efdc, res_fvcom),
               names_to = "Model",
               values_to = "Residuals") %>%
  dplyr::select(date_time, lat.x, lng, siteDepth, sampleDepth,
                Model, Residuals, Real_temp) %>%
  mutate(Model = case_when(Model == "res_efdc" ~ "EFDC",
                          Model == "res_fvcom" ~ "FVCOM"))
  
# residual plots (residuals over time)

pC <- ggplot(data = mvm_temp_res, aes(x = date_time, y = Residuals, color = Model)) + 
  theme_classic() + geom_point(alpha = 0.5) + 
  scale_y_continuous(breaks = c(-25,-20,-15,-10,-5,0,5,10,15,20,25), limits = c(-25,25)) +
  geom_abline(intercept = 0, slope = 0, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colvar) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%d") + 
  labs(x = "Date", y = "Residuals (Modeled - Measured)") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        legend.position = "bottom")
pC
ggsave(plot = pC, filename = "plots/fig4_C.png", width = 10, height = 6)

# combine temp over time figures

plot_grid(pA, pB, align = "h", nrow = 1, ncol = 2)
ggsave("plots/fig4_A_B.png", width = 10, height = 6)

################################################################################
#Part 4: CTD sites - depth comparison ####
# select a single node, all layers, all time points

######## Finding sites for figures (4 sites, 8 panel figure) ######## 
# subset ctd and wq data: 
ctd <- subset(temp.data, type == "ctd")

wq <- subset(temp.data, type == "wq")

#looking at distribution of temp data over time
ggplot(data = ctd, aes(x = date_time, y = temp)) + geom_point() + 
  scale_x_datetime(breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggplot(data = wq, aes(x = date_time, y = temp, color = siteID)) + geom_point() # beginning and end of simulation

#looking at data distribution across sites
ggplot(data = ctd, aes(x = siteID)) + geom_bar(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggplot(data = wq, aes(x = siteID)) + geom_bar(stat = "count") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
# most data (n > 15) = ON55M (site depth = 192.23 m, mid-lake east), ON33M (site depth = 137.75 m, mid-like west)

### WQ data

## ON55M 
on55m <- subset(temp.data, siteID == "ON55M")

midpoint <- min(on55m$temp) + (max(on55m$temp) - min(on55m$temp))/2

ggplot(data = on55m, aes(x = date_time, y = sampleDepth, color = temp)) + 
  geom_point(size = 2, alpha = 3, shape = 16) + scale_y_reverse() + 
  scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint) 

## ON33M 
on33m <- subset(temp.data, siteID == "ON33M")

midpoint <- min(on33m$temp) + (max(on33m$temp) - min(on33m$temp))/2

ggplot(data = on33m, aes(x = date_time, y = sampleDepth, color = temp)) + 
  geom_point(size = 2, alpha = 3, shape = 16) + scale_y_reverse() + 
  scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint) 

### CTD
# siteIDs by number of observations
site_counts <- table(ctd$siteID)

## Stn 33 (mid lake) - lots of casts, showing progression of stratification from may - oct
stn33 <- subset(temp.data, siteID == "stn33") 
midpoint <- min(stn33$temp) + (max(stn33$temp) - min(stn33$temp))/2

ggplot(data = stn33, aes(x = date_time, y = sampleDepth, color = temp)) + 
  geom_point(size = 2, alpha = 3, shape = 16) + scale_y_reverse() + 
  scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint) 

## Stn 41 (mid lake) - lots of data, casts early in may/jun show mixed water column and aug/sep casts show stratification
stn41 <- subset(temp.data, siteID == "stn41")
range(stn41$temp)
midpoint <- min(stn41$temp) + (max(stn41$temp) - min(stn41$temp))/2

ggplot() + geom_point(data = stn41, aes(x = date_time, y = sampleDepth, color = temp), size = 2, alpha = 3, shape = 16) + 
  scale_y_reverse() + scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint) 

## Stn 14 (west basin, offshore) - 1 cast in sep 
stn14 <- subset(temp.data, siteID == "stn14")
midpoint <- min(stn14$temp) + (max(stn14$temp) - min(stn14$temp))/2

ggplot() + geom_point(data = stn14, aes(x = date_time, y = sampleDepth, color = temp), size = 2, alpha = 3, shape = 16) + 
  scale_y_reverse() + scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint) 

## Stn 18 (west basin, offshore) - 2 casts - late July and mid Sep
stn18 <- subset(temp.data, siteID == "stn18")
midpoint <- min(stn18$temp) + (max(stn18$temp) - min(stn18$temp))/2

ggplot() + geom_point(data = stn18, aes(x = date_time, y = sampleDepth, color = temp), size = 2, alpha = 3, shape = 16) + 
  scale_y_reverse() + scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint) 

## Stn 716 (north mid lake) - 3 casts showing progression of stratificatoin*
stn716 <- subset(temp.data, siteID == "stn716")
midpoint <- min(stn716$temp) + (max(stn716$temp) - min(stn716$temp))/2

ggplot() + geom_point(data = stn716, aes(x = date_time, y = sampleDepth, color = temp), size = 2, alpha = 3, shape = 16) + 
  scale_y_reverse() + scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint) 

## NRM10 (near Niagara River) - two casts - July 27 and July 30
nrm10 <- subset(temp.data, siteID == "NRM10")
midpoint <- min(nrm10$temp) + (max(nrm10$temp) - min(nrm10$temp))/2

ggplot() + geom_point(data = nrm10, aes(x = date_time, y = sampleDepth, color = temp), size = 2, alpha = 3, shape = 16) + 
  scale_y_reverse() + scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint)

## NR10E15 (near Niagara River) - two casts - July 28 and July 30
nr10e15 <- subset(temp.data, siteID == "NR10E15")
midpoint <- min(nr10e15$temp) + (max(nr10e15$temp) - min(nr10e15$temp))/2

ggplot() + geom_point(data = nr10e15, aes(x = date_time, y = sampleDepth, color = temp), size = 2, alpha = 3, shape = 16) + 
  scale_y_reverse() + scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint)

## NRM25 (near Niagara River) - two casts - July 27 and July 30 (more data than nrm10)
nrm25 <- subset(temp.data, siteID == "NRM25")
midpoint <- min(nrm25$temp) + (max(nrm25$temp) - min(nrm25$temp))/2

ggplot() + geom_point(data = nrm25, aes(x = date_time, y = sampleDepth, color = temp), size = 2, alpha = 3, shape = 16) + 
  scale_y_reverse() + scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint)

## GRM40 (Ontario Bay) - best thermocline for Ontario Bay sites*
grm40 <- subset(temp.data, siteID == "GRM40")
range(grm40$temp)
midpoint <- min(grm40$temp) + (max(grm40$temp) - min(grm40$temp))/2

ggplot() + geom_point(data = grm40, aes(x = date_time, y = sampleDepth, color = temp), size = 2, alpha = 3, shape = 16) + 
  scale_y_reverse() + scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint) 
# one cast from 7/29 but good coverage over water column (maybe)

## GRM15 (Ontario Bay, near Genesee River)
grm15 <- subset(temp.data, siteID == "GRM15")
range(grm15$temp)
midpoint <- min(grm15$temp) + (max(grm15$temp) - min(grm15$temp))/2

ggplot() + geom_point(data = grm15, aes(x = date_time, y = sampleDepth, color = temp), size = 2, alpha = 3, shape = 16) + 
  scale_y_reverse() + scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint) 
# one cast from 7/29 but good coverage over water column (maybe)

## GRM25 (Ontario Bay, near Genesee River)
grm25 <- subset(temp.data, siteID == "GRM25")
range(grm25$temp)
midpoint <- min(grm25$temp) + (max(grm25$temp) - min(grm25$temp))/2

ggplot() + geom_point(data = grm25, aes(x = date_time, y = sampleDepth, color = temp), size = 2, alpha = 3, shape = 16) + 
  scale_y_reverse() + scale_color_gradient2(low = "cyan4", mid = "yellow", high = "red", midpoint = midpoint) 
# one cast from 7/29 but good coverage over water column (maybe)

######## Depth profile figures ######## 
# Sites: ON55M, ON33M, NRM25, GRM40

#### ON55M #### 
on55m <- subset(temp.data, siteID == "ON55M")
site <- "ON55M"
sitedepth <- unique(on55m$siteDepth)
p.title <- paste(site, "(43.44, -77.44)", sep = " ")

## FVCOM
node <- 19272
numL <- 20
numT <- 4393

model <- "FVCOM"
fill.title <- paste(model, "Temp °C", sep = " ")

fvcom.temp <- ncvar_get(fvcom, varid = "temp", start = c(node, 1, 1), count = c(1, numL, numT)) 
#dim(fvcom.temp) #20 rows by 4393 columns

fvcomTime <- data.frame(
  "days" = ncdf4::ncvar_get(fvcom, "time")) %>%
  mutate(time = seq_len(nrow(.))) %>%
  mutate(
    simDays = floor(days),
    simhours = hours(round((days - simDays) * 24)),
    simDays = days(simDays),
    simTime = simDays + simhours,
    dateTime = ymd("1970-01-01") + simTime,
  ) %>%
  select(time, dateTime)  

fvcom.depth <- ncvar_get(fvcom, varid = "h", start = node, count = 1) 

fvcom.site <- t(fvcom.temp) %>% 
  as.data.frame() %>%
  rownames_to_column(var = "time") %>%
  mutate(time = as.numeric(time)) %>%
  left_join(fvcomTime, by = "time") %>%
  pivot_longer(cols = -c(time, dateTime), names_to = "sigma", values_to = "temp") %>%
  mutate(siteID = site,
         siteDepth = fvcom.depth,
         nsigma = 20, 
         sigthick = fvcom.depth/20,
         sigma = as.numeric(gsub("[a-zA-Z]", "", sigma)),
         Depth = (sigma*sigthick - sigthick/2)) %>%
  select(siteID, dateTime, sigma, siteDepth, Depth, temp)

saveRDS(fvcom.site, "ON55M_fvcom.RData")
#midpoint temp for scale
midpoint <- min(fvcom.site$temp) + (max(fvcom.site$temp) - min(fvcom.site$temp))/2

# plot
custom.blue <- "#56B4E9"

p_hm1 <- ggplot() +
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = on55m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  geom_point(data = on55m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

#legend formatting (fill on top, color on bottom)
# extract legends
leg1 <- get_legend(p_hm1)
leg2 <- get_legend(p_hm2)

#create blank plot for legend alignment
blank_p <- plot_spacer() + theme_minimal()

#combine legends
leg12 <- plot_grid(leg1, leg2, blank_p, nrow = 3)

#add legends to plot 
p_hm3 <- plot_grid(p_hm0, leg12, nrow = 1, align = "h", axis = "t", rel_widths = c(1, 0.3))
p_hm3 # save as pdf to edit in illustrator

## EFDC
I <- 180
J <- 78
numL <- 10
numT <- 1464

model <- "EFDC"
fill.title <- paste(model, "Temp °C", sep = " ")

efdc.temp <- ncvar_get(efdc, varid = "Temp", start = c(I, J, 1, 1), count = c(1, 1, numL, numT))
efdc.time <- data.frame(dateTime = ncvar_get(efdc, varid = "Time") %>% as.POSIXct(tz = "GMT", origin = "1970-01-01")) %>%
  mutate(time = seq_len(nrow(.)))

efdc.site <- t(efdc.temp) %>%
  as.data.frame() %>%
  rownames_to_column(var = "time") %>%
  mutate(time = as.numeric(time)) %>%
  left_join(efdc.time, by = "time") %>%
  pivot_longer(cols = -c(time, dateTime), names_to = "sigma", values_to = "temp") %>%
  mutate(siteID = site,
         siteDepth = fvcom.depth,
         nsigma = 10, 
         sigthick = fvcom.depth/10,
         sigma = as.numeric(gsub("[a-zA-Z]", "", sigma)),
         Depth = (sigma*sigthick - sigthick/2)) %>%
  select(siteID, dateTime, sigma, siteDepth, Depth, temp)

saveRDS(efdc.site, "ON55M_efdc.RData")
# plot
custom.blue <- "#56B4E9"

p_hm1 <- ggplot() +
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = on55m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  geom_point(data = on55m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

#legend formatting (fill on top, color on bottom)
# extract legends
leg1 <- get_legend(p_hm1)
leg2 <- get_legend(p_hm2)

#create blank plot for legend alignment
blank_p <- plot_spacer() + theme_minimal()

#combine legends
leg12 <- plot_grid(leg1, leg2, blank_p, nrow = 3)

#add legends to plot 
p_hm3 <- plot_grid(p_hm0, leg12, nrow = 1, align = "h", axis = "t", rel_widths = c(1, 0.3))
p_hm3

#### ON33M #### 
on33m <- subset(temp.data, siteID == "ON33M")
site <- "ON33M"
p.title <- paste(site, "(43.60, -78.81)", sep = " ")

## FVCOM
node <- 8470
numL <- 20
numT <- 4393

model <- "FVCOM"
fill.title <- paste(model, "Temp °C", sep = " ")

fvcom.temp <- ncvar_get(fvcom, varid = "temp", start = c(node, 1, 1), count = c(1, numL, numT)) 
dim(fvcom.temp) #20 rows by 4393 columns

# only have to grab time var once
#fvcomTime <- data.frame(
#  "days" = ncdf4::ncvar_get(fvcom, "time")) %>%
#  mutate(time = seq_len(nrow(.))) %>%
#  mutate(
#    simDays = floor(days),
#    simhours = hours(round((days - simDays) * 24)),
#    simDays = days(simDays),
#    simTime = simDays + simhours,
#    dateTime = ymd("1970-01-01") + simTime,
#  ) %>%
#  select(time, dateTime)  

fvcom.depth <- ncvar_get(fvcom, varid = "h", start = node, count = 1) #water column depth - need to calculate output depths for each layer

fvcom.site <- t(fvcom.temp) %>% 
  as.data.frame() %>%
  rownames_to_column(var = "time") %>%
  mutate(time = as.numeric(time)) %>%
  left_join(fvcomTime, by = "time") %>%
  pivot_longer(cols = -c(time, dateTime), names_to = "sigma", values_to = "temp") %>%
  mutate(siteID = site,
         siteDepth = fvcom.depth,
         nsigma = 20, 
         sigthick = fvcom.depth/20,
         sigma = as.numeric(gsub("[a-zA-Z]", "", sigma)),
         Depth = (sigma*sigthick - sigthick/2)) %>%
  select(siteID, dateTime, sigma, siteDepth, Depth, temp)
saveRDS(fvcom.site, "ON33M_fvcom.RData")
#midpoint temp for scale
midpoint <- min(fvcom.site$temp) + (max(fvcom.site$temp) - min(fvcom.site$temp))/2

# plot
custom.blue <- "#56B4E9"

p_hm1 <- ggplot() +
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = on33m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  geom_point(data = on33m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

#legend formatting (fill on top, color on bottom)
# extract legends
leg1 <- get_legend(p_hm1)
leg2 <- get_legend(p_hm2)

#create blank plot for legend alignment
blank_p <- plot_spacer() + theme_minimal()

#combine legends
leg12 <- plot_grid(leg1, leg2, blank_p, nrow = 3)

#add legends to plot 
p_hm3 <- plot_grid(p_hm0, leg12, nrow = 1, align = "h", axis = "t", rel_widths = c(1, 0.3))
p_hm3

## EFDC
I <- 97
J <- 90
numL <- 10
numT <- 1464

model <- "EFDC"
fill.title <- paste(model, "Temp °C", sep = " ")

efdc.temp <- ncvar_get(efdc, varid = "Temp", start = c(I, J, 1, 1), count = c(1, 1, numL, numT))

# only have to grab time var once
#efdc.time <- data.frame(dateTime = ncvar_get(efdc, varid = "Time") %>% as.POSIXct(tz = "GMT", origin = "1970-01-01")) %>%
#  mutate(time = seq_len(nrow(.)))

efdc.site <- t(efdc.temp) %>%
  as.data.frame() %>%
  rownames_to_column(var = "time") %>%
  mutate(time = as.numeric(time)) %>%
  left_join(efdc.time, by = "time") %>%
  pivot_longer(cols = -c(time, dateTime), names_to = "sigma", values_to = "temp") %>%
  mutate(siteID = site,
         siteDepth = fvcom.depth,
         nsigma = 10, 
         sigthick = fvcom.depth/10,
         sigma = as.numeric(gsub("[a-zA-Z]", "", sigma)),
         Depth = (sigma*sigthick - sigthick/2)) %>%
  select(siteID, dateTime, sigma, siteDepth, Depth, temp)
saveRDS(efdc.site, "ON33M_efdc.RData")
#plot
p_hm1 <- ggplot() +
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = on33m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  geom_point(data = on33m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

#legend formatting (fill on top, color on bottom)
# extract legends
leg1 <- get_legend(p_hm1)
leg2 <- get_legend(p_hm2)

#create blank plot for legend alignment
blank_p <- plot_spacer() + theme_minimal()

#combine legends
leg12 <- plot_grid(leg1, leg2, blank_p, nrow = 3)

#add legends to plot 
p_hm3 <- plot_grid(p_hm0, leg12, nrow = 1, align = "h", axis = "t", rel_widths = c(1, 0.3))
p_hm3

#### NRM25 #### 
nrm25 <- subset(temp.data, siteID == "NRM25")
site <- "NRM25"
p.title <- paste(site, "(43.32, -79.04)", sep = " ")

## FVCOM
node <- 5963
numL <- 20
numT <- 4393

model <- "FVCOM"
fill.title <- paste(model, "Temp °C", sep = " ")

fvcom.temp <- ncvar_get(fvcom, varid = "temp", start = c(node, 1, 1), count = c(1, numL, numT)) 
dim(fvcom.temp) #20 rows by 4393 columns

# only have to grab time var once
#fvcomTime <- data.frame(
#  "days" = ncdf4::ncvar_get(fvcom, "time")) %>%
#  mutate(time = seq_len(nrow(.))) %>%
#  mutate(
#    simDays = floor(days),
#    simhours = hours(round((days - simDays) * 24)),
#    simDays = days(simDays),
#    simTime = simDays + simhours,
#    dateTime = ymd("1970-01-01") + simTime,
#  ) %>%
#  select(time, dateTime)  

fvcom.depth <- ncvar_get(fvcom, varid = "h", start = node, count = 1) #water column depth - need to calculate output depths for each layer

fvcom.site <- t(fvcom.temp) %>% 
  as.data.frame() %>%
  rownames_to_column(var = "time") %>%
  mutate(time = as.numeric(time)) %>%
  left_join(fvcomTime, by = "time") %>%
  pivot_longer(cols = -c(time, dateTime), names_to = "sigma", values_to = "temp") %>%
  mutate(siteID = site,
         siteDepth = fvcom.depth,
         nsigma = 20, 
         sigthick = fvcom.depth/20,
         sigma = as.numeric(gsub("[a-zA-Z]", "", sigma)),
         Depth = (sigma*sigthick - sigthick/2)) %>% #to get model output to start at 0 m
  select(siteID, dateTime, sigma, siteDepth, Depth, temp)
saveRDS(fvcom.site, "NRM25_fvcom.RData")
#midpoint temp for scale
midpoint <- min(fvcom.site$temp) + (max(fvcom.site$temp) - min(fvcom.site$temp))/2

#plot
p_hm1 <- ggplot() +
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = nrm25, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  geom_point(data = nrm25, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

#legend formatting (fill on top, color on bottom)
# extract legends
leg1 <- get_legend(p_hm1)
leg2 <- get_legend(p_hm2)

#create blank plot for legend alignment
blank_p <- plot_spacer() + theme_minimal()

#combine legends
leg12 <- plot_grid(leg1, leg2, blank_p, nrow = 3)

#add legends to plot 
p_hm3 <- plot_grid(p_hm0, leg12, nrow = 1, align = "h", axis = "t", rel_widths = c(1, 0.3))
p_hm3

## EFDC
I <- 72
J <- 73
numL <- 10
numT <- 1464

model <- "EFDC"
fill.title <- paste(model, "Temp °C", sep = " ")

efdc.temp <- ncvar_get(efdc, varid = "Temp", start = c(I, J, 1, 1), count = c(1, 1, numL, numT))

# only have to grab time var once
#efdc.time <- data.frame(dateTime = ncvar_get(efdc, varid = "Time") %>% as.POSIXct(tz = "GMT", origin = "1970-01-01")) %>%
#  mutate(time = seq_len(nrow(.)))

efdc.site <- t(efdc.temp) %>%
  as.data.frame() %>%
  rownames_to_column(var = "time") %>%
  mutate(time = as.numeric(time)) %>%
  left_join(efdc.time, by = "time") %>%
  pivot_longer(cols = -c(time, dateTime), names_to = "sigma", values_to = "temp") %>%
  mutate(siteID = site,
         siteDepth = fvcom.depth,
         nsigma = 10, 
         sigthick = fvcom.depth/10,
         sigma = as.numeric(gsub("[a-zA-Z]", "", sigma)),
         Depth = (sigma*sigthick - sigthick/2)) %>%
  select(siteID, dateTime, sigma, siteDepth, Depth, temp)
saveRDS(efdc.site, "NRM25_efdc.RData")
#plot
p_hm1 <- ggplot() +
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = nrm25, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  geom_point(data = nrm25, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

#legend formatting (fill on top, color on bottom)
# extract legends
leg1 <- get_legend(p_hm1)
leg2 <- get_legend(p_hm2)

#create blank plot for legend alignment
blank_p <- plot_spacer() + theme_minimal()

#combine legends
leg12 <- plot_grid(leg1, leg2, blank_p, nrow = 3)

#add legends to plot 
p_hm3 <- plot_grid(p_hm0, leg12, nrow = 1, align = "h", axis = "t", rel_widths = c(1, 0.3))
p_hm3

#### GRM40 #### 
grm40 <- subset(temp.data, siteID == "GRM40")
site <- "GRM40"
p.title <- paste(site, "(43.34, -77.57)", sep = " ")

## FVCOM
node <- 15490
numL <- 20
numT <- 4393

model <- "FVCOM"
fill.title <- paste(model, "Temp °C", sep = " ")

fvcom.temp <- ncvar_get(fvcom, varid = "temp", start = c(node, 1, 1), count = c(1, numL, numT)) 
dim(fvcom.temp) #20 rows by 4393 columns

# only have to grab time var once
#fvcomTime <- data.frame(
#  "days" = ncdf4::ncvar_get(fvcom, "time")) %>%
#  mutate(time = seq_len(nrow(.))) %>%
#  mutate(
#    simDays = floor(days),
#    simhours = hours(round((days - simDays) * 24)),
#    simDays = days(simDays),
#    simTime = simDays + simhours,
#    dateTime = ymd("1970-01-01") + simTime,
#  ) %>%
#  select(time, dateTime)  

fvcom.depth <- ncvar_get(fvcom, varid = "h", start = node, count = 1) #water column depth - need to calculate output depths for each layer

fvcom.site <- t(fvcom.temp) %>% 
  as.data.frame() %>%
  rownames_to_column(var = "time") %>%
  mutate(time = as.numeric(time)) %>%
  left_join(fvcomTime, by = "time") %>%
  pivot_longer(cols = -c(time, dateTime), names_to = "sigma", values_to = "temp") %>%
  mutate(siteID = site,
         siteDepth = fvcom.depth,
         nsigma = 20, 
         sigthick = fvcom.depth/20,
         sigma = as.numeric(gsub("[a-zA-Z]", "", sigma)),
         Depth = (sigma*sigthick) - sigthick/2) %>%
  select(siteID, dateTime, sigma, siteDepth, Depth, temp)
saveRDS(fvcom.site, "GRM40_fvcom.RData")
#midpoint temp for scale
midpoint <- min(fvcom.site$temp) + (max(fvcom.site$temp) - min(fvcom.site$temp))/2

#plot
p_hm1 <- ggplot() +
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = grm40, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  geom_point(data = grm40, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

#legend formatting (fill on top, color on bottom)
# extract legends
leg1 <- get_legend(p_hm1)
leg2 <- get_legend(p_hm2)

#create blank plot for legend alignment
blank_p <- plot_spacer() + theme_minimal()

#combine legends
leg12 <- plot_grid(leg1, leg2, blank_p, nrow = 3)

#add legends to plot 
p_hm3 <- plot_grid(p_hm0, leg12, nrow = 1, align = "h", axis = "t", rel_widths = c(1, 0.3))
p_hm3

## EFDC
I <- 173
J <- 59
numL <- 10
numT <- 1464

model <- "EFDC"
fill.title <- paste(model, "Temp °C", sep = " ")

efdc.temp <- ncvar_get(efdc, varid = "Temp", start = c(I, J, 1, 1), count = c(1, 1, numL, numT))

# only have to grab time var once
#efdc.time <- data.frame(dateTime = ncvar_get(efdc, varid = "Time") %>% as.POSIXct(tz = "GMT", origin = "1970-01-01")) %>%
#  mutate(time = seq_len(nrow(.)))

efdc.site <- t(efdc.temp) %>%
  as.data.frame() %>%
  rownames_to_column(var = "time") %>%
  mutate(time = as.numeric(time)) %>%
  left_join(efdc.time, by = "time") %>%
  pivot_longer(cols = -c(time, dateTime), names_to = "sigma", values_to = "temp") %>%
  mutate(siteID = site,
         siteDepth = fvcom.depth,
         nsigma = 10, 
         sigthick = fvcom.depth/10,
         sigma = as.numeric(gsub("[a-zA-Z]", "", sigma)),
         Depth = (sigma*sigthick - sigthick/2)) %>%
  select(siteID, dateTime, sigma, siteDepth, Depth, temp)
saveRDS(efdc.site, "GRM40_efdc.RData")
#plot
p_hm1 <- ggplot() +
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = grm40, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low =custom.blue, mid = "yellow", high = "red", midpoint = midpoint) +
  geom_point(data = grm40, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = custom.blue, mid = "yellow", high = "red", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

#legend formatting (fill on top, color on bottom)
# extract legends
leg1 <- get_legend(p_hm1)
leg2 <- get_legend(p_hm2)

#create blank plot for legend alignment
blank_p <- plot_spacer() + theme_minimal()

#combine legends
leg12 <- plot_grid(leg1, leg2, blank_p, nrow = 3)

#add legends to plot 
p_hm3 <- plot_grid(p_hm0, leg12, nrow = 1, align = "h", axis = "t", rel_widths = c(1, 0.3))
p_hm3
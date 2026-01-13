# Merging LOEM and LOTP observational data
# Author: Julie Maurer
# Created: 10/1/25

# NOTE: had previously done this in excel, which worked fine except duplicates remained and I want a logical, reproducable workflow

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/TPData_processing/")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)
library(geosphere)

#### processing LOEM data ####
loem.data <- read.csv("../LOEM/LOEM_TPdata_ModelYears.csv", header = TRUE)

loem.data <- loem.data %>% 
  mutate(date_time = as.Date(date_time, format = "%m/%d/%Y")) %>%
  mutate(Year = year(date_time)) %>%
  mutate(Model_source = "LOEM") %>%
  mutate(TP_ugL.1 = TP_mgL.1*1000) %>% # for some reason tp ug/L was in mg/L
  filter(date_time >= "2013-04-01" & date_time <= "2013-10-01" |
           date_time >= "2018-04-01" & date_time <= "2018-10-01") %>% #only include data within simulation 
  subset(samp_type_id == 3) %>% # remove type 2 (depth integrated) samples
  filter(depth_start <= 220 & depth_end <= 220) %>% #remove 999 and 996 depths (not real depths)
  select(date_time, Year, Longitude, Latitude, station_id, Agency, Program, LOEM_grid_fine, #reorder columns, depth_start = depth_end = sample depth
         LOEM_lyr_fine, TP_ugL.1, depth_start, Model_source) %>% 
  rename("node_EFDC" = LOEM_grid_fine,
         "K_Index" = LOEM_lyr_fine,
         "sample_depth" = depth_start) 

#### processing LOTP data ####
lotp.data <- read.csv("../LOTP/LOTP_ObservationalData.csv", header = TRUE)

lotp.data <- lotp.data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(Year = year(Date)) %>%
  mutate(Model_source = "LOTP") %>%
  filter(Date >= "2013-04-01" & Date <= "2013-10-01" |
           Date >= "2018-04-01" & Date <= "2018-10-01") %>%
  select(Date, Year, Longitude, Latitude, Source, I_Index, 
         J_Index, K_Index, TP_ugL.1, Number_Layers, Sigma_Layer_Thickness,
         Sample_Depth, Water_Depth, Model_source) %>%
  rename("date_time" = Date,
         "Agency" = Source,
         "sample_depth" = Sample_Depth,
         "water_depth" = Water_Depth)

#merging data frames
obsdata <- full_join(lotp.data, loem.data, 
                     by = c("date_time", "Year", "Longitude", "Latitude", "Agency", "K_Index",
                              "TP_ugL.1", "sample_depth", "Model_source"))
summary(is.na(obsdata))

obsdata_clean <- obsdata %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>% # remove 3 missing lat lons
  mutate(lon = round(Longitude, digits = 3)) %>%  # create rounded lat lon columns to 3 places
  mutate(lat = round(Latitude, digits = 3)) %>%
  group_by(lon, lat) %>% # filling in missing I,J index and water depth info for LOEM based on lat lon
  tidyr::fill(I_Index, .direction = "down") %>%
  tidyr::fill(J_Index, .direction = "down") %>%
  tidyr::fill(Number_Layers, .direction = "down") %>%
  tidyr::fill(Sigma_Layer_Thickness, .direction = "down") %>%
  tidyr::fill(water_depth, .direction = "down") %>%
  tidyr::fill(node_EFDC, .direction = "up") %>%
  dplyr::select(date_time, Year, Longitude, Latitude, lon, lat, Agency, Program, 
                station_id, node_EFDC, I_Index, J_Index, K_Index, Number_Layers, 
                Sigma_Layer_Thickness, sample_depth, water_depth, TP_ugL.1, Model_source)

# removing replicate samples (matching date, lon/lat, k index, TP, and sample depth)
obsdata_sub <- obsdata_clean %>% distinct(date_time, lon, lat, K_Index, TP_ugL.1, sample_depth, .keep_all = TRUE)

# many samples still have missing information

write.csv(obsdata_sub, "TP_observationaldata_combined_redo.csv")

saveRDS(obsdata_clean, "obsdata_clean.RData")
saveRDS(obsdata_sub, "obsdata_subset.RData")

#### adding missing efdc indicies and nodes ####

efdcPos <- read_table("/work/GLFBREEZ/Lake_Ontario/LatLon.txt") %>%
  mutate(node = seq_len(nrow(.))) %>%
  left_join(read_table("/work/GLFBREEZ/Lake_Ontario/IJ_Depth_NZ.txt", 
                       col_names = c("I", "J", "depth", "maxSigma")))

saveRDS(efdcPos, "efdcPos.RData")

obsdata_sub <- readRDS("obsdata_subset.RData")
efdcPos <- readRDS("efdcPos.RData")

efdcPos <- efdcPos %>% 
  mutate(lat2 = round(lat, digits = 2),
         lon2 = round(lon, digits = 2))

obsdata_sub <- obsdata_sub %>%
  mutate(lat = round(Latitude, digits = 2),
         lon = round(Longitude, digits = 2))

# gathering missing efdc nodes
obsdata_sub <- obsdata_sub %>% 
  left_join(efdcPos, by = c("I_Index" = "I", "J_Index" = "J")) %>%
  mutate(node_EFDC = coalesce(node_EFDC, node)) 

# gathering missing I and J indicies
obsdata_sub <- obsdata_sub %>% 
  left_join(efdcPos, by = c("node_EFDC" = "node")) %>%
  mutate(I_Index = coalesce(I_Index, I),
         J_Index = coalesce(J_Index, J)) 

# finish cleaning data

obsdata_sub <- obsdata_sub[,1:19] 
obsdata_sub <- obsdata_sub %>% rename("lon" = lon.x, 
                                      "lat" = lat.x) 
obsdata_sub <- obsdata_sub %>% 
  distinct(date_time, lon, lat, node_EFDC, I_Index, J_Index, 
           K_Index, TP_ugL.1, sample_depth, .keep_all = TRUE) %>% #remove remaining duplicates
  group_by(node_EFDC) %>% # filling in missing I,J index and water depth info for LOEM based on lat lon
  tidyr::fill(Program, .direction = "down") %>%
  tidyr::fill(station_id, .direction = "down") %>%
  tidyr::fill(Number_Layers, .direction = "down") %>%
  tidyr::fill(Sigma_Layer_Thickness, .direction = "down") %>%
  tidyr::fill(water_depth, .direction = "down")

saveRDS(obsdata_sub, "obsdata_subset.RData")
write.csv(obsdata_sub, "TP_observationaldata_combined_redo.csv")

#### adding fvcom nodes ####

fvcom.grid <- read.csv("FVCOM_nodes_2018inits.csv", header = TRUE)
obsdata_sub <- read.csv("TP_observationaldata_combined_redo.csv", header = TRUE)

#source: https://stackoverflow.com/questions/57525670/find-closest-points-lat-lon-from-one-data-set-to-a-second-data-set

df.A <- data.frame(lat = obsdata_sub$Latitude, lon = obsdata_sub$Longitude, node = obsdata_sub$node_EFDC)
df.B <- data.frame(lat = fvcom.grid$lat, lon = fvcom.grid$lng, node = fvcom.grid$node)

for(i in 1:nrow(df.A)) {
  #calculate distance against all of B (this could take a while)
  distances <- geosphere::distGeo(df.A[i,], df.B)/1000
  
  #rank the calculated distances
  ranking <- rank(distances, ties.method = "first")
  
  #find shortest and store the indexes of B in A
  df.A$shortest[i] <- which(ranking == 1)
  
  #store distances in A
  df.A$shortestD[i] <- distances[df.A$shortest[i]]
}

write.csv(df.A, "fvcom-nodemap-geosphere.csv")

# merging nodes 
obsdata_sub2 <- obsdata_sub %>% 
  left_join(df.A, by = c("Latitude" = "lat", "Longitude" = "lon", "node_EFDC" = "node")) %>%
  rename("node_fvcom" = "shortest",
         "distance_efdc-fvcom_nodes" = "shortestD",
         "nLayers_efdc" = "Number_Layers",
         "sigthick_efdc" = "Sigma_Layer_Thickness")

#calculate sigma layer from sample depth divided by layer thickness:

obsdata_sub2$nLayers_fvcom <- 20

obsdata_sub2$sigthick_fvcom <- obsdata_sub2$water_depth/20

obsdata_sub2$sigma <- floor(obsdata_sub2$sample_depth / obsdata_sub2$sigthick_fvcom)

obsdata_sub2$sigma <- as.integer(obsdata_sub2$sigma)

# Replace 0 -> 1 and >20 -> 20 
obsdata_sub2$sigma <- replace(obsdata_sub2$sigma, obsdata_sub2$sigma > 20, 20)
obsdata_sub2$sigma <- replace(obsdata_sub2$sigma, obsdata_sub2$sigma < 1, 1)

obsdata_sub3 <- obsdata_sub2 %>% 
  distinct(date_time, lon, lat, node_EFDC, I_Index, J_Index, K_Index, 
           sample_depth, node_fvcom, .keep_all = TRUE) %>% #removed duplicates 
  select(date_time, Year, Longitude, Latitude, Agency, Program, station_id, node_fvcom, nLayers_fvcom, sigthick_fvcom, 
         sigma, node_EFDC, nLayers_efdc, sigthick_efdc, I_Index, J_Index, K_Index, sample_depth, water_depth, TP_ugL.1, Model_source)

write.csv(obsdata_sub3, "../TPData_processing/TP_observationaldata_combined_efdc-fvcom.csv")
saveRDS(obsdata_sub3, "../TPData_processing/obsdata_combined_efdc-fvcom.RData")

# missing water depths remain - fill in manually in arcGIS
obsdata_sub4 <- read.csv("../TPData_processing/TP_observationaldata_combined_efdc-fvcom.csv") 

# remove Hamilton Harbor points
# I = 15 -> 9
obsdata_sub5 <- obsdata_sub4[obsdata_sub4$I_Index > 15,]
obsdata_sub5$date_time <- as.Date(obsdata_sub5$date_time, "%m/%d/%Y")

write.csv(obsdata_sub5, "../TPData_processing/TP_observationaldata_combined_noHH_efdc-fvcom.csv")
saveRDS(obsdata_sub5,  "../TPData_processing/obsdata_combined_efdc-fvcom_final.RData")





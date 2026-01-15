# Merging temperature data (LOEM + GLENDA)
# Author: Julie Maurer
# Created: 10/16/25

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/TPData_processing/")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)
library(readxl)

# read in GLENDA data (already processed - see /work/GLHABS/GreatLakesHydro/LakeOntario/scripts/04-observationaldata for details)
temp.data <- read.csv("../validation-data/WQ_Data_combined_temp-tp-chl.csv")

temp.data <- temp.data %>% 
  dplyr::select(-c(tp, chla)) %>%
  mutate(date_time = as.Date(date_time, format = "%Y-%m-%d"))

# read in LOEM data
parameters <- read_xlsx("../validation-data/LOEM/m_parameters.xlsx")
values <- read_xlsx("../validation-data/LOEM/m_results.xlsx")
sites <- read_xlsx("../validation-data/LOEM/m_stations.xlsx")
programs <- read_xlsx("../validation-data/LOEM/m_programs.xlsx")

View(parameters) # temp is param # 128

# temp_new_station and station_no are the same for temp
setequal(values$station_no[values$param_no == 128], values$temp_new_station[values$param_no == 128])

setequal(values$Result, values$result_disp) #same

df.siteinfo <- sites %>% 
  left_join(programs, by = "program_no") %>%
  filter(program_name != "Observed") %>%  # remove 101: all lumped together with no station data, causes problems down the line
  dplyr::select(station_no, station_id, Latitude, Longitude, program_no, 
         program_name, LOEM_grid_fine, Station_Type) %>%
  mutate(station_no = as.character(station_no))

df.temp <- values %>% 
  subset(param_no == 128) %>%
  left_join(df.siteinfo, by = c("true_station_no" = "station_no")) %>%
  dplyr::select(date_time, Latitude, Longitude, samp_type_id, program_name, station_id, 
         depth_start, Result, LOEM_lyr_fine, LOEM_grid_fine) %>%
  rename("lat" = Latitude, "lng" = Longitude, "type" = samp_type_id, 
         "source" = program_name, "siteID" = station_id, "sampleDepth" = depth_start,
         "temp" = Result, "K_Index" = LOEM_lyr_fine, "node_efdc" = LOEM_grid_fine) %>%
  mutate(date_time = as.Date(date_time, format = "%Y-%m-%d")) %>%
  filter(date_time >= "2013-04-01" & date_time <= "2013-10-01" |
           date_time >= "2018-04-01" & date_time <= "2018-10-01") %>%
  mutate(type = case_when(type == "3" ~ "wq"))

# combine LOEM and GLENDA data
temp.data.all <- temp.data %>%
  left_join(df.temp, by = c("date_time", "lat", "lng", "type", "siteID", "sampleDepth",
                            "temp", "node_efdc", "source")) %>%
  dplyr::select(date_time, lat, lng, type, source, siteID, siteDepth, sampleDepth, temp,
                node_fvcom, sigma, node_efdc, I_Index, J_Index, K_Index, dist_fvcom, 
                dist_efdc)

saveRDS(temp.data.all, "Temp_data_combined.RData")

# add efdc sigma layer from efdcP
temp.data.all <- readRDS("Temp_data_combined.RData")
efdcPos <- readRDS("efdcPos.RData")

temp.data.all <- temp.data.all %>%
  left_join(efdcPos, by = c("node_efdc" = "node", "I_Index" = "I", "J_Index" = "J")) %>%
  rename("nLayers_efdc" = maxSigma) %>%
  select(-c(XCent, YCent, lat.y, lon, depth))

# calculate sigma layer efdc from num layers and site depth
temp.data.all <- temp.data.all %>%
  mutate(sigthick_efdc = siteDepth/nLayers_efdc,
         K_Index = floor(sampleDepth / sigthick_efdc)) %>%
  select(-c(sigthick_efdc))

# Replace 0 -> 1 and >10 -> 10
temp.data.all$K_Index <- replace(temp.data.all$K_Index, temp.data.all$K_Index > 10, 10)
temp.data.all$K_Index <- replace(temp.data.all$K_Index, temp.data.all$K_Index < 1, 1) 

summary(is.na(temp.data.all)) #missing quite a few site depths and a few sample depths

#remove missing temp data
temp.data.all2 <- temp.data.all[!is.na(temp.data.all$temp),]
summary(is.na(temp.data.all2))
#adding site depth manually from arcgis grid
write.csv(temp.data.all2, "Temp_observationaldata_combined_loem-fvcom.csv")

temp.data.all2 <- read.csv("Temp_observationaldata_combined_loem-fvcom.csv")

# recalculate sigma layer efdc from num layers and site depth
temp.data.all2 <- temp.data.all2 %>%
  mutate(sigthick_efdc = siteDepth/nLayers_efdc,
         K_Index = floor(sampleDepth / sigthick_efdc)) %>%
  select(-c(sigthick_efdc))

write.csv(temp.data.all2, "Temp_observationaldata_combined_loem-fvcom.csv")

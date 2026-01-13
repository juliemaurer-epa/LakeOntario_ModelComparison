## Nearshore zone statistics
# Date created: 11/13/25

library(tidyverse)
library(dplyr)
library(ggplot2)
#install.packages("ggpattern")
library(ggpattern) #for this to work, must load following modules before launching R studio: gdal, geos, hdf5, netcdf, proj, udunits

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/")

#### Zones, all depths #####
## read in text files & format

medTP_EFDC1 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_EFDCS1.txt",
                          sep = ",",
                          header = TRUE) %>%
  pivot_longer(cols = c(1:4),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "EFDC-LOTPM",
         Scenario = "Scenario1",
         MedianTP = MedianTP*1e6) #convert to ug/L

medTP_EFDC2 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_EFDCS2.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:4),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "EFDC-LOTPM",
         Scenario = "Scenario2",
         MedianTP = MedianTP*1e6) #convert to ug/L

medTP_EFDC3 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_EFDCS3.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:4),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "EFDC-LOTPM",
         Scenario = "Scenario3",
         MedianTP = MedianTP*1e6) #convert to ug/L

medTP_LOEM1 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_LOEMS1.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:4),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "LOEM",
         Scenario = "Scenario1",
         MedianTP = MedianTP*1e3) #convert to ug/L 

medTP_LOEM2 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_LOEMS2.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:4),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "LOEM",
         Scenario = "Scenario2",
         MedianTP = MedianTP*1e3) #convert to ug/L 

medTP_LOEM3 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_LOEMS3.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:4),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "LOEM",
         Scenario = "Scenario3",
         MedianTP = MedianTP*1e3) #convert to ug/L 

medTP_FVCOM1 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_FVCOMS1.txt", 
                           sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:4),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "FVCOM-LOTPM",
         Scenario = "Scenario1") 

medTP_FVCOM2 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_FVCOMS2.txt", 
                           sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:4),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "FVCOM-LOTPM",
         Scenario = "Scenario2") 

## combine dataframes
medianTP <- medTP_EFDC1 %>%
  full_join(medTP_EFDC2, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_EFDC3, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_LOEM1, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_LOEM2, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_LOEM3, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_FVCOM1, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_FVCOM2, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  mutate(Scenario = factor(Scenario, levels = c("Scenario1", "Scenario2", "Scenario3")))

## add median observed TP
data <- read.csv("/work/GLHABS/GreatLakesEco/LakeOntario/TPData_processing/TP_observationaldata_combined_noHH_efdc-fvcom.csv")

# zone 1:
E_I <- seq(87,106, by = 1)
E_J <- seq(48, 69, by = 1)

# zone 2:
H_I <- seq(146, 153, by = 1)
H_J <- seq(48, 65, by = 1)

# zone 3:
O_I <- seq(170, 174, by = 1)
O_J <- seq(11, 49, by = 1)

# zone 4
Off_I <- seq (93, 172, by = 1)
Off_J <- seq(75, 84, by = 1)

data2 <- data

data2$zone <- ifelse(data2$I_Index %in% E_I & data2$J_Index %in% E_J, "Eighteenmile_Creek",
                     ifelse(data2$I_Index %in% H_I & data2$J_Index %in% H_J, "Hamlin_Beach",
                            ifelse(data2$I_Index %in% O_I & data2$J_Index %in% O_J, "Ontario_Beach",
                                   ifelse(data2$I_Index %in% Off_I & data2$J_Index %in% Off_J, "Offshore_Zone",
                                          NA))))

write.csv(data2, "/work/GLHABS/GreatLakesEco/LakeOntario/TPData_processing/TP_observationaldata_combined_noHH_zones.csv")

data2 <- read.csv("/work/GLHABS/GreatLakesEco/LakeOntario/TPData_processing/TP_observationaldata_combined_noHH_zones.csv")

med1 <- median(data2$TP_ugL.1[data2$zone == "Eighteenmile_Creek"], na.rm = TRUE)
med2 <- median(data2$TP_ugL.1[data2$zone == "Hamlin_Beach"], na.rm = TRUE)
med3 <- median(data2$TP_ugL.1[data2$zone == "Ontario_Beach"], na.rm = TRUE)
med4 <- median(data2$TP_ugL.1[data2$zone == "Offshore_Zone"], na.rm = TRUE)

medianData <- data.frame(Zone = c("Eigtheenmile_Creek", "Hamlin_Beach", "Ontario_Beach", "Offshore_Zone"),
                         MedianTP = c(med1, med2, med3, med4),
                         Model = "Data",
                         Scenario = "none")

# add to medianTP and save
medianTP2 <- medianTP %>%
  full_join(medianData, by = c("Zone", "MedianTP", "Model", "Scenario"))

write.csv(medianTP2, "medianTP_zones_combined.csv")


#### Zones,by depth #####

## read in text files & format

medTP_EFDC1 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_5m_30m_EFDCS1.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:12),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "EFDC-LOTPM",
         Scenario = "Scenario1",
         MedianTP = MedianTP*1e6) #convert to ug/L

medTP_EFDC2 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_5m_30m_EFDCS2.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:12),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "EFDC-LOTPM",
         Scenario = "Scenario2",
         MedianTP = MedianTP*1e6) #convert to ug/L

medTP_EFDC3 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_5m_30m_EFDCS3.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:12),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "EFDC-LOTPM",
         Scenario = "Scenario3",
         MedianTP = MedianTP*1e6) #convert to ug/L

medTP_LOEM1 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_5m_30m_LOEMS1.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:12),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "LOEM",
         Scenario = "Scenario1",
         MedianTP = MedianTP*1e3) #convert to ug/L 

medTP_LOEM2 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_5m_30m_LOEMS2.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:12),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "LOEM",
         Scenario = "Scenario2",
         MedianTP = MedianTP*1e3) #convert to ug/L 

medTP_LOEM3 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_5m_30m_LOEMS3.txt", 
                          sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:12),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "LOEM",
         Scenario = "Scenario3",
         MedianTP = MedianTP*1e3) #convert to ug/L 

medTP_FVCOM1 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_5m_30m_FVCOMS1.txt", 
                           sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:12),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "FVCOM-LOTPM",
         Scenario = "Scenario1",
         MedianTP = MedianTP*1e3) #convert to ug/L 

medTP_FVCOM2 <- read.table(file = "/work/GLFBREEZ/LOEM/MATLAB_Scripts/manuscript-figs/median_TP_zones_5m_30m_FVCOMS2.txt", 
                           sep = ",", header = TRUE) %>%
  pivot_longer(cols = c(1:12),
               names_to = "Zone",
               values_to = "MedianTP") %>%
  mutate(Model = "FVCOM-LOTPM",
         Scenario = "Scenario2",
         MedianTP = MedianTP*1e3) #convert to ug/L 

## combine dataframes

medianTP_depths <- medTP_EFDC1 %>%
  full_join(medTP_EFDC2, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_EFDC3, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_LOEM1, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_LOEM2, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_LOEM3, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_FVCOM1, by = c("Zone", "MedianTP", "Model", "Scenario")) %>%
  full_join(medTP_FVCOM2, by = c("Zone", "MedianTP", "Model", "Scenario"))

medianTP_depths <- medianTP_depths %>%
  mutate(DepthInt = case_when(
    grepl("_5m_30m", Zone) ~ "5-30m",
    grepl("_30m", Zone) ~ ">30m",
    grepl("_5m", Zone) ~ "<5m")) %>%
  mutate(ZoneName = case_when(
    grepl("Eighteenmile", Zone) ~ "Eighteenmile Creek",
    grepl("Hamlin", Zone) ~ "Hamlin Beach",
    grepl("Ontario", Zone) ~ "Ontario Beach",
    grepl("Offshore", Zone) ~ "Offshore"))

## add median observed TP
med1_5m <- median(data2$TP_ugL.1[data2$zone == "Eighteenmile_Creek" & 
                                   data2$water_depth < 5], na.rm = TRUE)
med1_5_30m <- median(data2$TP_ugL.1[data2$zone == "Eighteenmile_Creek" & 
                                   data2$water_depth > 5 & data2$water_depth < 30], na.rm = TRUE)
med1_30m <- median(data2$TP_ugL.1[data2$zone == "Eighteenmile_Creek" & 
                                   data2$water_depth > 30], na.rm = TRUE)

med2_5m <- median(data2$TP_ugL.1[data2$zone == "Hamlin_Beach" & 
                                   data2$water_depth < 5], na.rm = TRUE)
med2_5_30m <- median(data2$TP_ugL.1[data2$zone == "Hamlin_Beach" & 
                                      data2$water_depth > 5 & data2$water_depth < 30], na.rm = TRUE)
med2_30m <- median(data2$TP_ugL.1[data2$zone == "Hamlin_Beach" & 
                                   data2$water_depth > 30], na.rm = TRUE)

med3_5m <- median(data2$TP_ugL.1[data2$zone == "Ontario_Beach" & 
                                   data2$water_depth < 5], na.rm = TRUE)
med3_5_30m <- median(data2$TP_ugL.1[data2$zone == "Ontario_Beach" & 
                                   data2$water_depth > 5 & data2$water_depth < 30], na.rm = TRUE)
med3_30m <- median(data2$TP_ugL.1[data2$zone == "Ontario_Beach" & 
                                   data2$water_depth > 30], na.rm = TRUE)

med4_5m <- median(data2$TP_ugL.1[data2$zone == "Offshore_Zone" & 
                                   data2$water_depth < 5], na.rm = TRUE)
med4_5_30m <- median(data2$TP_ugL.1[data2$zone == "Offshore_Zone" & 
                                   data2$water_depth > 5 & data2$water_depth < 30], na.rm = TRUE)
med4_30m <- median(data2$TP_ugL.1[data2$zone == "Offshore_Zone" & 
                                    data2$water_depth > 30], na.rm = TRUE)

medianData_depths <- data.frame(Zone = c("Eighteenmile_Creek_5m", "Eighteenmile_Creek_5m_30m", "Eighteenmile_Creek_30m",
                                         "Hamlin_Beach_5m", "Hamlin_Beach_5m_30m", "Hamlin_Beach_30m", "Ontario_Beach_5m",
                                         "Ontario_Beach_5m_30m", "Ontario_Beach_30m", "Offshore_Zone_5m", 
                                         "Offshore_Zone_5m_30m", "Offshore_Zone_30m"),
                         MedianTP = c(med1_5m, med1_5_30m, med1_30m, 
                                      med2_5m, med2_5_30m, med2_30m,
                                      med3_5m, med3_5_30m, med3_30m, 
                                      med4_5m, med4_5_30m, med4_30m),
                         Model = "Data",
                         Scenario = "none",
                         DepthInt = c("<5m", "5-30m", ">30m", "<5m", "5-30m", ">30m", 
                                      "<5m", "5-30m", ">30m", "<5m", "5-30m", ">30m"),
                         ZoneName = c("Eighteenmile Creek", "Eighteenmile Creek", "Eighteenmile Creek",
                                      "Hamlin Beach", "Hamlin Beach", "Hamlin Beach", 
                                      "Ontario Beach", "Ontario Beach", "Ontario Beach", 
                                      "Offshore", "Offshore", "Offshore"))

medianTP_depths2 <- medianTP_depths %>%
  full_join(medianData_depths, by = c("Zone", "MedianTP", "Model", "Scenario", "DepthInt", "ZoneName"))

# save df
write.csv(medianTP_depths2, "MedianTP_zones_depths.csv")

## plot
medianTP_depths2 <- read.csv("MedianTP_zones_depths.csv")

medianTP_depths2 <- medianTP_depths2 %>%
  mutate(Model = factor(Model, levels = c("Data", "FVCOM-LOTPM", "EFDC-LOTPM", "LOEM")),
         Scenario = factor(Scenario, levels = c("none", "Scenario1", "Scenario2", "Scenario3")),
         DepthInt = factor(DepthInt, levels = c("<5m", "5-30m", ">30m")),
         ZoneName = factor(ZoneName, levels = c("Eighteenmile Creek", "Hamlin Beach", "Ontario Beach", "Offshore"))
  ) %>%
  drop_na() %>%
  mutate(Scenario = case_when(
    Scenario == "none" ~ "Data",
    Scenario == "Scenario1" ~ "Scenario 1",
    Scenario == "Scenario2" ~ "Scenario 2",
    Scenario == "Scenario3" ~ "Scenario 3"
  ))

fillvals <- c("#999999", #data
             "#E69F00", #scenario 1
             "#56B4E9", #scenario 2
             "#009E73") #scenario 3

patternvals <- c("none", #data
                 "stripe", #fvcom
                 "crosshatch", #efdc
                 "circle") #loem

pz <- ggplot(medianTP_depths2, aes(x = DepthInt, y = MedianTP, fill = Scenario, pattern = Model)) + 
  geom_bar_pattern(stat = "identity", position = position_dodge(preserve = "single"), colour = "black") + 
  scale_fill_manual(values = fillvals) + 
  scale_pattern_manual(values = patternvals) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  facet_wrap(~ZoneName, scales = "free_x") + theme_classic() + 
  labs(x = "Depth Interval", y = "Median TP Concentration (ug L-1)") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        strip.text = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"))
pz

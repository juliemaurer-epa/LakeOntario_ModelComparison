# LOEM analysis
# Date created: 11/12/25

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison")

library(ncdf4)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot)

## Potential analysis:
# 1. Further analysis of P fractions simulated by LOEM - which P species has the biggest fraction of TP? 
# 2. Comparison of TP fractions with obs data: TP, DPO4/SRP, TDP (total dissolved P), TPP (total particulate P)
# 3. Chlorophyll analysis - where does the model show chl-a accumulating? behavior over time? (obs data to compare)
# 4. Cladophora analysis - does TP accumulation seem to correlate with Cladophora biomass? (no obs data)

#### LOEM scenario 1 ####
loem1 <- "/work/GLFBREEZ/LOEM/LOEM_Winmodel_nc/Scenario1/LOEM_2018_scenario1.nc"

nc <- nc_open(loem1)

time <- ncvar_get(nc, varid = "datetime") %>% # seconds since 1970-01-01
  as.POSIXct(origin = "1970-01-01") %>%
  as.data.frame() %>%
  mutate(timestep = row_number()) %>%
  rename("date" = ".")

# Re-arrange 4D array into 2D dataframe (time by grid cell) for each variable as it's imported 
tp <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "TP")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, TP = value)

ldop <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "LDOP")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, LDOP = value) #labile dissolved organic

lpip <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "LPIP")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, LPIP = value) #labile particulate inorganic

lpop <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "LPOP")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, LPOP = value) # labile particulate organic

dpo4 <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "DPO4")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, DPO4 = value) # SRP

po4t <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "PO4T")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, PO4T = value) # total phosphate (including in cells)

rdop <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "RDOP")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, RDOP = value) # refractory dissolved organic

rpip <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "RPIP")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, RPIP = value) # refractory particulate inorganic

rpop <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "RPOP")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, RPOP = value) # refractory particulate organic

chla <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "CHLAVE")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, CHLAVE = value) # average chl-a

do <- reshape2::melt(ncdf4::ncvar_get(nc, varid = "DOAVE")) %>%
  dplyr::rename(I = Var1, J = Var2, K = Var3, time = Var4, DOAVE = value) # average dissolved oxygen

nc_close(nc)

# combine into monster df - did this in base R (don't run interactively or program will crash!)
efdcPos <- readRDS("../TPData_processing/efdcPos.RData")

loem.vars <- tp %>%
  left_join(efdcPos) %>%
  left_join(time, by = c("time" = "timestep")) %>%
  select(-c(XCent, YCent)) %>%
  rename(lng= lon) %>%
  arrange(lat, lng) %>%
  left_join(ldop, by = c("I", "J", "K", "time")) %>%
  left_join(lpip, by = c("I", "J", "K", "time")) %>%
  left_join(lpop, by = c("I", "J", "K", "time")) %>%
  left_join(dpo4, by = c("I", "J", "K", "time")) %>%
  left_join(po4t, by = c("I", "J", "K", "time")) %>%
  left_join(rdop, by = c("I", "J", "K", "time")) %>%
  left_join(rpip, by = c("I", "J", "K", "time")) %>%
  left_join(rpop, by = c("I", "J", "K", "time")) %>%
  left_join(chla, by = c("I", "J", "K", "time")) %>%
  left_join(do, by = c("I", "J", "K", "time")) %>%
  drop_na()
  
saveRDS(loem.vars, "loem_vars.RData")

#removing -9999 and -9999000 - haven't run yet
loem.vars2 <- loem.vars %>%
  filter(TP != -9999,
         LDOP != -9999,
         LPIP != -9999,
         LPOP != -9999,
         DPO4 != -9999,
         PO4T != -9999,
         RDOP != -9999,
         RPIP != -9999,
         RPOP != -9999,
         CHLAVE != -9999,
         DOAVE != -9999)

# SRP = DPO4 (param # = 104)

# total dissolved P = LDOP + RDOP + DPO4 (param # = 103)

# total particulate P = LPIP + LPOP + RPIP + RPOP (param # = 130)

# average chlorophyll = CHLAVE (param # = 108)

# dissolved oxygen = DOAVE (param # = 101)


################################################################################
# P Fractions
################################################################################

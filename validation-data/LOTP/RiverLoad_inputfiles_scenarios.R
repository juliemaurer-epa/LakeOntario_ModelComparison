### Creating river load input files for model scenarios
# Models: EFDC TP and FVCOM TP *LOEM inputs will be in a separate script

#packages
library(dplyr)
library(tidyr)
library(openxlsx)

##  NOTES:
################################################################################

##  EFDC TP model formatting notes:
# Input files used to update the .nc are in wide format
# Input data: time variable TP load from Jan 1st - Dec 31st 
# Units: kg/s

##  Scenario 1 - LOEM load for all major rivers
#EFDC TP 2013: LOTP_EFDC_2013_input_scenario1.csv 
################################################################################
output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/LOTP_EFDC_2013_input_scenario1.csv"
input_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOEM/LOEM data processing/LOEM_TP_Loads_2013/LOEM_TP_Daily_Loads_2013.xlsx"

loem.loads <- read_xlsx(input_file, sheet = "TP_Loads_g_s_modperiod")

str(loem.loads)
#View(loem.loads)

loem.loads$Date <- as.Date(loem.loads$Date, "%Y-%m-%d")
# remove St Lawrence 

loem.loads <- subset(loem.loads, select = -c(St_Lawrence1, St_Lawrence2))

##  Steps:
# date range for input file: 4/1/13 - 10/1/13 - EFDC TP model needs 1/1/13 - 12/31/13
dfLD <- data.frame(Date = seq(as.Date("2013/1/1"), by = "day", length.out = 365),
                   Niagara = 0.0, Eighteenmile = 0.0, OakOrchard = 0.0, Genesee = 0.0, Oswego = 0.0)
#View(dfLD)

# copy first and last TP value, extend to fill needed date range
dayNumbers <- match(loem.loads$Date, dfLD$Date)

# Assign River loads to the corresponding day numbers in data frame.
#	Rivers are in specific sequence: 1 = Niagara, 2 = Eighteenmile, 3 = Oak Orchard, 4 = Genesee, 5 = Oswego
#	TP Load is in kg/s

dfLD$Niagara[dayNumbers] <- loem.loads$Niagara 
dfLD$Eighteenmile[dayNumbers] <- loem.loads$Eighteenmile
dfLD$OakOrchard[dayNumbers] <- loem.loads$Oak_Orchard
dfLD$Genesee[dayNumbers] <- loem.loads$Genesee
dfLD$Oswego[dayNumbers] <- loem.loads$Oswego

# Repeat the April 1st TP load for the January 1 - March 31 time frame.
date1 <- match("2013-01-01", dfLD$Date)
date2 <- match("2013-03-31", dfLD$Date)
date3 <- match("2013-04-01", dfLD$Date)
dfLD$Niagara[date1:date2] <- dfLD$Niagara[date3]
dfLD$Eighteenmile[date1:date2] <- dfLD$Eighteenmile[date3]
dfLD$OakOrchard[date1:date2] <- dfLD$OakOrchard[date3]
dfLD$Genesee[date1:date2] <- dfLD$Genesee[date3]
dfLD$Oswego[date1:date2] <- dfLD$Genesee[date3]
#View(dfLD)

# convert units from g/s to kg/s
dfLD.long <- dfLD %>% pivot_longer(cols = -Date,
                                   names_to = "River",
                                   values_to = "TP_gs")
dfLD.long$TP_kgs <- dfLD.long$TP_gs / 1000
dfLD.long2 <- subset(dfLD.long, select = -TP_gs)
#View(dfLD.long2)

dfLD2 <- dfLD.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
#View(dfLD2)

write.csv(dfLD2, output_file)

################################################################################
#EFDC TP 2018: LOTP_EFDC_2018_input_scenario1.csv
################################################################################
output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/LOTP_EFDC_2018_input_scenario1.csv"
input_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOEM/LOEM data processing/LOEM_TP_Loads_2018/LOEM_TP_Daily_Loads_2018.xlsx"

loem.loads <- read.xlsx(input_file, sheet = "TP_Loads_g_s", detectDates = TRUE, colNames = TRUE)

str(loem.loads)
#View(loem.loads)

loem.loads$Date <- as.Date(loem.loads$Date, "%Y-%m-%d")

# remove other rivers 
#5/29 note - adding Black River to our model
loem.loads <- subset(loem.loads, select = -c(St_Lawrence1, St_Lawrence2, Salmon))

##  Steps:
# date range for input file: 4/1/13 - 10/1/13 - EFDC TP model needs 1/1/13 - 12/31/13
dfLD <- data.frame(Date = seq(as.Date("2018-01-01"), by = "day", length.out = 365),
                   Niagara = 0.0, Eighteenmile = 0.0, OakOrchard = 0.0, Genesee = 0.0, 
                   Oswego = 0.0, Black = 0.0)
#View(dfLD)

# copy first and last TP value, extend to fill needed date range
dayNumbers <- match(loem.loads$Date, dfLD$Date)

# Assign River loads to the corresponding day numbers in data frame.
#	Rivers are in specific sequence: 1 = Niagara, 2 = Eighteenmile, 3 = Oak Orchard, 4 = Genesee, 5 = Oswego
#	TP Load is in kg/s

dfLD$Niagara[dayNumbers] <- loem.loads$Niagara 
dfLD$Eighteenmile[dayNumbers] <- loem.loads$"18_Mile_Creek"
dfLD$OakOrchard[dayNumbers] <- loem.loads$Oak_Orchard
dfLD$Genesee[dayNumbers] <- loem.loads$Genesee
dfLD$Oswego[dayNumbers] <- loem.loads$Oswego
dfLD$Black[dayNumbers] <- loem.loads$Black

# Repeat the April 1st TP load for the January 1 - March 31 time frame.
date1 <- match("2018-01-01", dfLD$Date)
date2 <- match("2018-03-31", dfLD$Date)
date3 <- match("2018-04-01", dfLD$Date)
dfLD$Niagara[date1:date2] <- dfLD$Niagara[date3]
dfLD$Eighteenmile[date1:date2] <- dfLD$Eighteenmile[date3]
dfLD$OakOrchard[date1:date2] <- dfLD$OakOrchard[date3]
dfLD$Genesee[date1:date2] <- dfLD$Genesee[date3]
dfLD$Oswego[date1:date2] <- dfLD$Oswego[date3]
dfLD$Black[date1:date2] <- dfLD$Black[date3]

date4 <- match("2018-10-02", dfLD$Date)
date5 <- match("2018-12-31", dfLD$Date)
date6 <- match("2018-10-01", dfLD$Date)
dfLD$Niagara[date4:date5] <- dfLD$Niagara[date6]
dfLD$Eighteenmile[date4:date5] <- dfLD$Eighteenmile[date6]
dfLD$OakOrchard[date4:date5] <- dfLD$OakOrchard[date6]
dfLD$Genesee[date4:date5] <- dfLD$Genesee[date6]
dfLD$Oswego[date4:date5] <- dfLD$Oswego[date6]
dfLD$Black[date4:date5] <- dfLD$Black[date6]
View(dfLD)

# convert units from g/s to kg/s
dfLD.long <- dfLD %>% pivot_longer(cols = -Date,
                                   names_to = "River",
                                   values_to = "TP_gs")
dfLD.long$TP_kgs <- dfLD.long$TP_gs / 1000
dfLD.long2 <- subset(dfLD.long, select = -TP_gs)
#View(dfLD.long2)

dfLD2 <- dfLD.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
View(dfLD2)

write.csv(dfLD2, output_file)

################################################################################
##  Scenario 2 - LOEM loads for all major rivers, but use NOTL-LDT values for Niagara
################################################################################
#EFDC TP 2013: LOTP_EFDC_2013_input_scenario2.csv 
################################################################################

output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/LOTP_EFDC_2013_input_scenario2.csv"
input_loem <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOEM/LOEM data processing/LOEM_TP_Loads_2013/LOEM_TP_Daily_Loads_2013.xlsx"

loem.loads <- read_xlsx(input_loem, sheet = "TP_Loads_g_s_modperiod")

str(loem.loads)
#View(loem.loads)
loem.loads$Date <- as.Date(loem.loads$Date, "%Y-%m-%d")
# remove St Lawrence 
loem.loads <- subset(loem.loads, select = -c(St_Lawrence1, St_Lawrence2))

input_notl <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Niagara_River_2013.txt"

input_notl <- read.table(input_notl,
                       header = FALSE)
names(input_notl) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
input_notl$dateTime <- as.Date(with(input_notl, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.notl <- data.frame(siteName = "Niagara", 
                      dateTime = input_notl$dateTime, 
                      flow = input_notl$Flow_cfs*0.0283, 
                      TP = input_notl$TP_mgL)
str(df.notl)
df.notl$Load <- df.notl$flow*df.notl$TP

##  Steps:
# date range for input file: 4/1/13 - 10/1/13 - EFDC TP model needs 1/1/13 - 12/31/13
dfLD <- data.frame(Date = seq(as.Date("2013/1/1"), by = "day", length.out = 365),
                   Niagara = 0.0, Eighteenmile = 0.0, OakOrchard = 0.0, Genesee = 0.0, Oswego = 0.0)
#View(dfLD)

# copy first and last TP value, extend to fill needed date range
dayNumbers <- match(loem.loads$Date, dfLD$Date)

# Assign River loads to the corresponding day numbers in data frame.
#	Rivers are in specific sequence: 1 = Niagara, 2 = Eighteenmile, 3 = Oak Orchard, 4 = Genesee, 5 = Oswego
#	TP Load is in kg/s

dfLD$Niagara[dayNumbers] <- loem.loads$Niagara 
dfLD$Eighteenmile[dayNumbers] <- loem.loads$Eighteenmile
dfLD$OakOrchard[dayNumbers] <- loem.loads$Oak_Orchard
dfLD$Genesee[dayNumbers] <- loem.loads$Genesee
dfLD$Oswego[dayNumbers] <- loem.loads$Oswego

# Repeat the April 1st TP load for the January 1 - March 31 time frame.
date1 <- match("2013-01-01", dfLD$Date)
date2 <- match("2013-03-31", dfLD$Date)
date3 <- match("2013-04-01", dfLD$Date)
dfLD$Niagara[date1:date2] <- dfLD$Niagara[date3]
dfLD$Eighteenmile[date1:date2] <- dfLD$Eighteenmile[date3]
dfLD$OakOrchard[date1:date2] <- dfLD$OakOrchard[date3]
dfLD$Genesee[date1:date2] <- dfLD$Genesee[date3]
dfLD$Oswego[date1:date2] <- dfLD$Genesee[date3]
View(dfLD)

# Repeat the OCt 1st TP load for the Oct 2nd - Dec 31st timeframe
date4 <- match("2013-12-31", dfLD$Date)
date5 <- match("2013-10-02", dfLD$Date)
date6 <- match("2013-10-01", dfLD$Date)
dfLD$Niagara[date4:date5] <- dfLD$Niagara[date6]
dfLD$Eighteenmile[date4:date5] <- dfLD$Eighteenmile[date6]
dfLD$OakOrchard[date4:date5] <- dfLD$OakOrchard[date6]
dfLD$Genesee[date4:date5] <- dfLD$Genesee[date6]
dfLD$Oswego[date4:date5] <- dfLD$Genesee[date6]
View(dfLD)

#Switch Niagara load with NOTL-LDT values

dfLD$Niagara <- df.notl$Load

# convert units from g/s to kg/s
dfLD.long <- dfLD %>% pivot_longer(cols = -Date,
                                   names_to = "River",
                                   values_to = "TP_gs")
dfLD.long$TP_kgs <- dfLD.long$TP_gs / 1000
dfLD.long2 <- subset(dfLD.long, select = -TP_gs)
#View(dfLD.long2)

dfLD2 <- dfLD.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
#View(dfLD2)

write.csv(dfLD2, output_file)

################################################################################
#EFDC TP 2018: LOTP_EFDC_2018_input_scenario2.csv 
################################################################################

output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/LOTP_EFDC_2018_input_scenario2.csv"
input_loem <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOEM/LOEM data processing/LOEM_TP_Loads_2018/LOEM_TP_Daily_Loads_2018.xlsx"

loem.loads <- read_xlsx(input_loem, sheet = "TP_Loads_g_s")

str(loem.loads)
#View(loem.loads)
loem.loads$Date <- as.Date(loem.loads$Date, "%Y-%m-%d")
# remove St Lawrence 
loem.loads <- subset(loem.loads, select = -c(St_Lawrence1, St_Lawrence2, Salmon))

input_notl <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Niagara_River_2018.txt"

input_notl <- read.table(input_notl,
                         header = FALSE)
names(input_notl) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")

input_notl$dateTime <- as.Date(with(input_notl, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.notl <- data.frame(siteName = "Niagara", 
                      dateTime = input_notl$dateTime, 
                      flow = input_notl$Flow_cfs*0.0283, 
                      TP = input_notl$TP_mgL)
df.notl$Load <- df.notl$flow*df.notl$TP
str(df.notl)

##  Steps:
# date range for input file: 4/1/13 - 10/1/13 - EFDC TP model needs 1/1/13 - 12/31/13
dfLD <- data.frame(Date = seq(as.Date("2018/1/1"), by = "day", length.out = 365),
                   Niagara = 0.0, Eighteenmile = 0.0, OakOrchard = 0.0, Genesee = 0.0, Oswego = 0.0, Black = 0.0)
str(dfLD)
str(loem.loads)

# copy first and last TP value, extend to fill needed date range
dayNumbers <- match(loem.loads$Date, dfLD$Date)

# Assign River loads to the corresponding day numbers in data frame.
#	Rivers are in specific sequence: 1 = Niagara, 2 = Eighteenmile, 3 = Oak Orchard, 4 = Genesee, 5 = Oswego
#	TP Load is in kg/s

dfLD$Niagara[dayNumbers] <- loem.loads$Niagara
dfLD$Eighteenmile[dayNumbers] <- loem.loads$'18_Mile_Creek'
dfLD$OakOrchard[dayNumbers] <- loem.loads$Oak_Orchard
dfLD$Genesee[dayNumbers] <- loem.loads$Genesee
dfLD$Oswego[dayNumbers] <- loem.loads$Oswego
dfLD$Black[dayNumbers] <- loem.loads$Black

# Repeat the April 1st TP load for the January 1 - March 31 time frame.
date1 <- match("2018-01-01", dfLD$Date)
date2 <- match("2018-03-31", dfLD$Date)
date3 <- match("2018-04-01", dfLD$Date)
dfLD$Niagara[date1:date2] <- dfLD$Niagara[date3]
dfLD$Eighteenmile[date1:date2] <- dfLD$Eighteenmile[date3]
dfLD$OakOrchard[date1:date2] <- dfLD$OakOrchard[date3]
dfLD$Genesee[date1:date2] <- dfLD$Genesee[date3]
dfLD$Oswego[date1:date2] <- dfLD$Genesee[date3]
dfLD$Black[date1:date2] <- dfLD$Black[date3]
#View(dfLD)

# Repeat the OCt 1st TP load for the Oct 2nd - Dec 31st timeframe
date4 <- match("2018-12-31", dfLD$Date)
date5 <- match("2018-10-02", dfLD$Date)
date6 <- match("2018-10-01", dfLD$Date)
dfLD$Niagara[date4:date5] <- dfLD$Niagara[date6]
dfLD$Eighteenmile[date4:date5] <- dfLD$Eighteenmile[date6]
dfLD$OakOrchard[date4:date5] <- dfLD$OakOrchard[date6]
dfLD$Genesee[date4:date5] <- dfLD$Genesee[date6]
dfLD$Oswego[date4:date5] <- dfLD$Oswego[date6]
dfLD$Black[date4:date5] <- dfLD$Black[date6]
View(dfLD)

#Switch Niagara load with NOTL-LDT values

dfLD$Niagara <- df.notl$Load

# convert units from g/s to kg/s
dfLD.long <- dfLD %>% pivot_longer(cols = -Date,
                                   names_to = "River",
                                   values_to = "TP_gs")
dfLD.long$TP_kgs <- dfLD.long$TP_gs / 1000
dfLD.long2 <- subset(dfLD.long, select = -TP_gs)
#View(dfLD.long2)

dfLD2 <- dfLD.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
#View(dfLD2)

write.csv(dfLD2, output_file)
################################################################################
#Scenario 3 (Updated): LOEM loads for all rivers, but 50% reduction in Genesee
#EFDC TP 2018: LOTP_EFDC_2013_input_scenario3_v2.csv 
################################################################################
output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/LOTP_EFDC_2018_input_scenario3_v2.csv"
input_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOEM/LOEM data processing/LOEM_TP_Loads_2018/LOEM_TP_Daily_Loads_2018.xlsx"

loem.loads <- read.xlsx(input_file, sheet = "TP_Loads_g_s", detectDates = TRUE, colNames = TRUE)

str(loem.loads)
#View(loem.loads)
loem.loads$Date <- as.Date(loem.loads$Date, "%Y-%m-%d")
# remove other rivers 
#5/29 note - adding Black River to our model
loem.loads <- subset(loem.loads, select = -c(St_Lawrence1, St_Lawrence2, Salmon))

#Genesee - 50% of concentration x flow

loem.flow <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOEM/LOEM data processing/LOEM_TP_Loads_2018/LOEM_River_Flows_2018.xlsx"

df.flow <- read.xlsx(xlsxFile = loem.flow, 
                     sheet = "Flows",
                     colNames = TRUE)

df.flow <- df.flow[1:184,]
df.flow$Date <- df.load$Date

df.tp <- data.frame(Date = loem.loads$Date,
                    Niagara = loem.loads$Niagara/df.flow$Niagara,
                    Eighteenmile = loem.loads$'18_Mile_Creek'/df.flow$'18_Mile_Creek',
                    OakOrchard = loem.loads$Oak_Orchard/df.flow$Oak_Orchard,
                    Genesee = loem.loads$Genesee/df.flow$Genesee,
                    Oswego = loem.loads$Oswego/df.flow$Oswego,
                    Black = loem.loads$Black/df.flow$Black)

df.genesee <- data.frame(Date = df.tp$Date, 
                         TP_mgL = df.tp$Genesee, 
                         Flow = df.flow$Genesee)
df.genesee$TP50 <- df.genesee$TP_mgL/2
df.genesee$Load50 <- df.genesee$TP50 * df.genesee$Flow
head(df.genesee)

#replace 50% reduction load with Genesee River 
loem.loads$Genesee50 <- df.genesee$Load50

##  Steps:
# date range for input file: 4/1/13 - 10/1/13 - EFDC TP model needs 1/1/13 - 12/31/13
dfLD <- data.frame(Date = seq(as.Date("2018-01-01"), by = "day", length.out = 365),
                   Niagara = 0.0, Eighteenmile = 0.0, OakOrchard = 0.0, Genesee = 0.0, 
                   Oswego = 0.0, Black = 0.0)
#View(dfLD)

# copy first and last TP value, extend to fill needed date range
dayNumbers <- match(loem.loads$Date, dfLD$Date)

# Assign River loads to the corresponding day numbers in data frame.
#	Rivers are in specific sequence: 1 = Niagara, 2 = Eighteenmile, 3 = Oak Orchard, 4 = Genesee, 5 = Oswego
#	TP Load is in kg/s

dfLD$Niagara[dayNumbers] <- loem.loads$Niagara 
dfLD$Eighteenmile[dayNumbers] <- loem.loads$'18_Mile_Creek'
dfLD$OakOrchard[dayNumbers] <- loem.loads$Oak_Orchard
dfLD$Genesee[dayNumbers] <- loem.loads$Genesee50
dfLD$Oswego[dayNumbers] <- loem.loads$Oswego
dfLD$Black[dayNumbers] <- loem.loads$Black

# Repeat the April 1st TP load for the January 1 - March 31 time frame.
date1 <- match("2018-01-01", dfLD$Date)
date2 <- match("2018-03-31", dfLD$Date)
date3 <- match("2018-04-01", dfLD$Date)
dfLD$Niagara[date1:date2] <- dfLD$Niagara[date3]
dfLD$Eighteenmile[date1:date2] <- dfLD$Eighteenmile[date3]
dfLD$OakOrchard[date1:date2] <- dfLD$OakOrchard[date3]
dfLD$Genesee[date1:date2] <- dfLD$Genesee[date3]
dfLD$Oswego[date1:date2] <- dfLD$Oswego[date3]
dfLD$Black[date1:date2] <- dfLD$Black[date3]

date4 <- match("2018-10-02", dfLD$Date)
date5 <- match("2018-12-31", dfLD$Date)
date6 <- match("2018-10-01", dfLD$Date)
dfLD$Niagara[date4:date5] <- dfLD$Niagara[date6]
dfLD$Eighteenmile[date4:date5] <- dfLD$Eighteenmile[date6]
dfLD$OakOrchard[date4:date5] <- dfLD$OakOrchard[date6]
dfLD$Genesee[date4:date5] <- dfLD$Genesee[date6]
dfLD$Oswego[date4:date5] <- dfLD$Oswego[date6]
dfLD$Black[date4:date5] <- dfLD$Black[date6]
View(dfLD)

# convert units from g/s to kg/s
dfLD.long <- dfLD %>% pivot_longer(cols = -Date,
                                   names_to = "River",
                                   values_to = "TP_gs")
dfLD.long$TP_kgs <- dfLD.long$TP_gs / 1000
dfLD.long2 <- subset(dfLD.long, select = -TP_gs)
#View(dfLD.long2)

dfLD2 <- dfLD.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
View(dfLD2)

write.csv(dfLD2, output_file)


################################################################################
#EFDC TP 2013: LOTP_EFDC_2013_input_scenario3.csv 
################################################################################
output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/LOTP_EFDC_2013_input_scenario3.csv"

eighteen <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/EighteenMile_River_2013.txt"
genesee <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Genessee_River_2013.txt"
oak <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Oakorchard_River_2013.txt"
oswego <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Oswego_River_2013.txt"

#Niagara - Dolan
mta <- 1744

load_mtd <- mta/365
load_mts <- load_mtd/(60*60*24)
load_gs <- load_mts * 1e+6

niagara.dolan <- data.frame(River = "Niagara",
                            dateTime = notl.ldt$dateTime, 
                            flow = notl.ldt$Flow_cfs*0.028, 
                            Load = load_gs)
head(niagara.dolan)

# Eighteenmile
eighteen <- read.table(file = eighteen,
                       header = FALSE)
names(eighteen) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
eighteen$dateTime <- as.Date(with(eighteen, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.eighteen <- data.frame(siteName = "Eighteenmile", dateTime = eighteen$dateTime, 
                          flow = (eighteen$Flow_cfs*0.0283), TP = eighteen$TP_mgL)
df.eighteen$Load <- df.eighteen$flow * df.eighteen$TP
head(df.eighteen)

#OakOrchard
oak <- read.table(file = oak,
                  header = FALSE)
names(oak) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oak$dateTime <- as.Date(with(oak, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

oakorchard <- data.frame(River = "OakOrchard", dateTime = oak$dateTime, 
                         flow = oak$Flow_cfs*0.0283, TP = oak$TP_mgL)
oakorchard$Load <- oakorchard$flow * oakorchard$TP
head(oakorchard)

#Genesee
genesee <- read.table(file = genesee,
                      header = FALSE)
names(genesee) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
genesee$dateTime <- as.Date(with(genesee, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.genesee <- data.frame(River = "Genesee", dateTime = genesee$dateTime, 
                         flow = genesee$Flow_cfs*0.0283, TP = genesee$TP_mgL)
df.genesee$Load <- df.genesee$flow*df.genesee$TP
head(df.genesee)

#Oswego
oswego <- read.table(file = oswego,
                     header = FALSE)
names(oswego) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oswego$dateTime <- as.Date(with(oswego, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.oswego <- data.frame(River = "Oswego", dateTime = oswego$dateTime, 
                        flow = oswego$Flow_cfs*0.0283, TP = oswego$TP_mgL)
df.oswego$Load <- df.oswego$flow * df.oswego$TP
head(df.oswego)

df.scenario3 <- data.frame(Date = niagara.dolan$dateTime, Niagara = niagara.dolan$Load, 
                           Eighteenmile = df.eighteen$Load, OakOrchard = oakorchard$Load,
                           Genesee = df.genesee$Load, Oswego = df.oswego$Load)
head(df.scenario3)

# convert units from g/s to kg/s
df.long <- df.scenario3 %>% pivot_longer(cols = -Date,
                                         names_to = "River",
                                         values_to = "TP_gs")
df.long$TP_kgs <- df.long$TP_gs / 1000
df.long2 <- subset(df.long, select = -TP_gs)

df2.scenario3 <- df.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
head(df2.scenario3)

write.csv(df2.scenario3, output_file)
################################################################################
#EFDC TP 2018: LOTP_EFDC_2018_input_scenario3.csv 
################################################################################

output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/LOTP_EFDC_2018_input_scenario3.csv"

eighteen <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/EighteenMile_River_2018.txt"
genesee <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Genessee_River_2018.txt"
oak <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Oakorchard_River_2018.txt"
oswego <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Oswego_River_2018.txt"
black <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Black_River_2018.txt"

#Niagara - Dolan
mta <- 1744

load_mtd <- mta/365
load_mts <- load_mtd/(60*60*24)
load_gs <- load_mts * 1e+6

niagara.dolan <- data.frame(River = "Niagara",
                            dateTime = notl.ldt$dateTime, 
                            flow = notl.ldt$Flow_cfs*0.028, 
                            Load = load_gs)
head(niagara.dolan)

# Eighteenmile
eighteen <- read.table(file = eighteen,
                       header = FALSE)
names(eighteen) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
eighteen$dateTime <- as.Date(with(eighteen, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.eighteen <- data.frame(siteName = "Eighteenmile", dateTime = eighteen$dateTime, 
                          flow = (eighteen$Flow_cfs*0.0283), TP = eighteen$TP_mgL)
df.eighteen$Load <- df.eighteen$flow * df.eighteen$TP
head(df.eighteen)

#OakOrchard
oak <- read.table(file = oak,
                  header = FALSE)
names(oak) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oak$dateTime <- as.Date(with(oak, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

oakorchard <- data.frame(River = "OakOrchard", dateTime = oak$dateTime, 
                         flow = oak$Flow_cfs*0.0283, TP = oak$TP_mgL)
oakorchard$Load <- oakorchard$flow * oakorchard$TP
head(oakorchard)

#Genesee
genesee <- read.table(file = genesee,
                      header = FALSE)
names(genesee) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
genesee$dateTime <- as.Date(with(genesee, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.genesee <- data.frame(River = "Genesee", dateTime = genesee$dateTime, 
                         flow = genesee$Flow_cfs*0.0283, TP = genesee$TP_mgL)
df.genesee$Load <- df.genesee$flow*df.genesee$TP
head(df.genesee)

#Oswego
oswego <- read.table(file = oswego,
                     header = FALSE)
names(oswego) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oswego$dateTime <- as.Date(with(oswego, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.oswego <- data.frame(River = "Oswego", dateTime = oswego$dateTime, 
                        flow = oswego$Flow_cfs*0.0283, TP = oswego$TP_mgL)
df.oswego$Load <- df.oswego$flow * df.oswego$TP
head(df.oswego)

#Black
black <- read.table(file = black,
                    header = FALSE)
names(black) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
black$dateTime <- as.Date(with(black, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.black <- data.frame(River = "Black", dateTime = black$dateTime,
                       flow = black$Flow_cfs*0.0283, TP = black$TP_mgL,
                       Load_mtd = black$Load_mtd)
head(df.black)
#convert load from mt/day to g/s
df.black$Load <- df.black$Load_mtd/(60*60*24) * 1e06

df.scenario3 <- data.frame(Date = df.eighteen$dateTime, Niagara = niagara.dolan$Load, 
                           Eighteenmile = df.eighteen$Load, OakOrchard = oakorchard$Load,
                           Genesee = df.genesee$Load, Oswego = df.oswego$Load, Black = df.black$Load)
head(df.scenario3)

# convert units from g/s to kg/s
df.long <- df.scenario3 %>% pivot_longer(cols = -Date,
                                         names_to = "River",
                                         values_to = "TP_gs")
df.long$TP_kgs <- df.long$TP_gs / 1000
df.long2 <- subset(df.long, select = -TP_gs)

df2.scenario3 <- df.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
head(df2.scenario3)

write.csv(df2.scenario3, output_file)

################################################################################
#EFDC TP 2013: LOTP_EFDC_2013_input_scenario4.csv 
################################################################################
output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/LOTP_EFDC_2013_input_scenario4.csv"

niagara <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Niagara_River_2013.txt"
eighteen <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/EighteenMile_River_2013.txt"
genesee <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Genessee_River_2013.txt"
oak <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Oakorchard_River_2013.txt"
oswego <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Oswego_River_2013.txt"

#Niagara - NOTL-LDT
niagara <- read.table(niagara,
                      header = FALSE)
names(niagara) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
niagara$dateTime <- as.Date(with(niagara, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.notl <- data.frame(siteName = "Niagara", 
                      dateTime = niagara$dateTime, 
                      flow = niagara$Flow_cfs*0.0283, 
                      TP = niagara$TP_mgL)
str(df.notl)
df.notl$Load <- df.notl$flow*df.notl$TP

# Eighteenmile
eighteen <- read.table(file = eighteen,
                       header = FALSE)
names(eighteen) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
eighteen$dateTime <- as.Date(with(eighteen, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.eighteen <- data.frame(siteName = "Eighteenmile", dateTime = eighteen$dateTime, 
                          flow = (eighteen$Flow_cfs*0.0283), TP = eighteen$TP_mgL)
df.eighteen$Load <- df.eighteen$flow * df.eighteen$TP
head(df.eighteen)

#OakOrchard
oak <- read.table(file = oak,
                  header = FALSE)
names(oak) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oak$dateTime <- as.Date(with(oak, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

oakorchard <- data.frame(River = "OakOrchard", dateTime = oak$dateTime, 
                         flow = oak$Flow_cfs*0.0283, TP = oak$TP_mgL)
oakorchard$Load <- oakorchard$flow * oakorchard$TP
head(oakorchard)

#Genesee
genesee <- read.table(file = genesee,
                      header = FALSE)
names(genesee) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
genesee$dateTime <- as.Date(with(genesee, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.genesee <- data.frame(River = "Genesee", dateTime = genesee$dateTime, 
                         flow = genesee$Flow_cfs*0.0283, TP = genesee$TP_mgL)
df.genesee$Load <- df.genesee$flow*df.genesee$TP
head(df.genesee)

#Oswego
oswego <- read.table(file = oswego,
                     header = FALSE)
names(oswego) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oswego$dateTime <- as.Date(with(oswego, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.oswego <- data.frame(River = "Oswego", dateTime = oswego$dateTime, 
                        flow = oswego$Flow_cfs*0.0283, TP = oswego$TP_mgL)
df.oswego$Load <- df.oswego$flow * df.oswego$TP
head(df.oswego)

df.scenario4 <- data.frame(Date = df.notl$dateTime, Niagara = df.notl$Load, 
                           Eighteenmile = df.eighteen$Load, OakOrchard = oakorchard$Load,
                           Genesee = df.genesee$Load, Oswego = df.oswego$Load)
head(df.scenario4)

# convert units from g/s to kg/s
df.long <- df.scenario4 %>% pivot_longer(cols = -Date,
                                         names_to = "River",
                                         values_to = "TP_gs")
df.long$TP_kgs <- df.long$TP_gs / 1000
df.long2 <- subset(df.long, select = -TP_gs)

df2.scenario4 <- df.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
head(df2.scenario4)

write.csv(df2.scenario4, output_file)

################################################################################
#EFDC TP 2018: LOTP_EFDC_2018_input_scenario4.csv 
################################################################################
output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/LOTP_EFDC_2018_input_scenario4.csv"

niagara <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Niagara_River_2018.txt"
eighteen <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/EighteenMile_River_2018.txt"
genesee <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Genessee_River_2013.txt"
oak <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Oakorchard_River_2013.txt"
oswego <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Oswego_River_2013.txt"
black <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Black_River_2018.txt"

#Niagara - NOTL-LDT
niagara <- read.table(niagara,
                      header = FALSE)
names(niagara) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
niagara$dateTime <- as.Date(with(niagara, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.notl <- data.frame(siteName = "Niagara", 
                      dateTime = niagara$dateTime, 
                      flow = niagara$Flow_cfs*0.0283, 
                      TP = niagara$TP_mgL)
df.notl$Load <- df.notl$flow*df.notl$TP
head(df.notl)

# Eighteenmile
eighteen <- read.table(file = eighteen,
                       header = FALSE)
names(eighteen) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
eighteen$dateTime <- as.Date(with(eighteen, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.eighteen <- data.frame(siteName = "Eighteenmile", dateTime = eighteen$dateTime, 
                          flow = (eighteen$Flow_cfs*0.0283), TP = eighteen$TP_mgL)
df.eighteen$Load <- df.eighteen$flow * df.eighteen$TP
head(df.eighteen)

#OakOrchard
oak <- read.table(file = oak,
                  header = FALSE)
names(oak) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oak$dateTime <- as.Date(with(oak, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

oakorchard <- data.frame(River = "OakOrchard", dateTime = oak$dateTime, 
                         flow = oak$Flow_cfs*0.0283, TP = oak$TP_mgL)
oakorchard$Load <- oakorchard$flow * oakorchard$TP
head(oakorchard)

#Genesee
genesee <- read.table(file = genesee,
                      header = FALSE)
names(genesee) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
genesee$dateTime <- as.Date(with(genesee, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.genesee <- data.frame(River = "Genesee", dateTime = genesee$dateTime, 
                         flow = genesee$Flow_cfs*0.0283, TP = genesee$TP_mgL)
df.genesee$Load <- df.genesee$flow*df.genesee$TP
head(df.genesee)

#Oswego
oswego <- read.table(file = oswego,
                     header = FALSE)
names(oswego) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oswego$dateTime <- as.Date(with(oswego, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.oswego <- data.frame(River = "Oswego", dateTime = oswego$dateTime, 
                        flow = oswego$Flow_cfs*0.0283, TP = oswego$TP_mgL)
df.oswego$Load <- df.oswego$flow * df.oswego$TP
head(df.oswego)

#Black
black <- read.table(file = black,
                    header = FALSE)
names(black) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
black$dateTime <- as.Date(with(black, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.black <- data.frame(River = "Black", dateTime = black$dateTime,
                       flow = black$Flow_cfs*0.0283, TP = black$TP_mgL,
                       Load_mtd = black$Load_mtd)
#convert load from mt/day to g/s
df.black$Load <- df.black$Load_mtd/(60*60*24) * 1e06

head(df.black)


df.scenario4 <- data.frame(Date = df.notl$dateTime, Niagara = df.notl$Load, 
                           Eighteenmile = df.eighteen$Load, OakOrchard = oakorchard$Load,
                           Genesee = df.genesee$Load, Oswego = df.oswego$Load, Black = df.black$Load)
head(df.scenario4)

# convert units from g/s to kg/s
df.long <- df.scenario4 %>% pivot_longer(cols = -Date,
                                         names_to = "River",
                                         values_to = "TP_gs")
df.long$TP_kgs <- df.long$TP_gs / 1000
df.long2 <- subset(df.long, select = -TP_gs)

df2.scenario4 <- df.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
head(df2.scenario4)

write.csv(df2.scenario4, output_file)

################################################################################
#EFDC TP 2013: LOTP_EFDC_2013_input_scenario5.csv 
################################################################################
output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/LOTP_EFDC_2013_input_scenario5.csv"

niagara <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Niagara_River_2013.txt"
eighteen <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/EighteenMile_River_2013.txt"
genesee <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Genessee_River_2013.txt"
oak <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Oakorchard_River_2013.txt"
oswego <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2013/textfiles_atmos/Oswego_River_2013.txt"

#Niagara - NOTL-LDT
niagara <- read.table(niagara,
                      header = FALSE)
names(niagara) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
niagara$dateTime <- as.Date(with(niagara, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.notl <- data.frame(siteName = "Niagara", 
                      dateTime = niagara$dateTime, 
                      flow = niagara$Flow_cfs*0.0283, 
                      TP = niagara$TP_mgL)
df.notl$Load <- df.notl$flow*df.notl$TP
head(df.notl)

# Eighteenmile
eighteen <- read.table(file = eighteen,
                       header = FALSE)
names(eighteen) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
eighteen$dateTime <- as.Date(with(eighteen, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.eighteen <- data.frame(siteName = "Eighteenmile", dateTime = eighteen$dateTime, 
                          flow = (eighteen$Flow_cfs*0.0283), TP = eighteen$TP_mgL)
df.eighteen$Load <- df.eighteen$flow * df.eighteen$TP
head(df.eighteen)

#OakOrchard
oak <- read.table(file = oak,
                  header = FALSE)
names(oak) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oak$dateTime <- as.Date(with(oak, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

oakorchard <- data.frame(River = "OakOrchard", dateTime = oak$dateTime, 
                         flow = oak$Flow_cfs*0.0283, TP = oak$TP_mgL)
oakorchard$Load <- oakorchard$flow * oakorchard$TP
head(oakorchard)

#Genesee - 50% reduction in concentration
genesee <- read.table(file = genesee,
                      header = FALSE)
names(genesee) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
genesee$dateTime <- as.Date(with(genesee, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.genesee <- data.frame(River = "Genesee", dateTime = genesee$dateTime, 
                         flow = genesee$Flow_cfs*0.0283, TP = genesee$TP_mgL/2)
df.genesee$Load <- df.genesee$flow*df.genesee$TP
head(df.genesee)

#Oswego
oswego <- read.table(file = oswego,
                     header = FALSE)
names(oswego) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oswego$dateTime <- as.Date(with(oswego, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.oswego <- data.frame(River = "Oswego", dateTime = oswego$dateTime, 
                        flow = oswego$Flow_cfs*0.0283, TP = oswego$TP_mgL)
df.oswego$Load <- df.oswego$flow * df.oswego$TP
head(df.oswego)

df.scenario5 <- data.frame(Date = df.notl$dateTime, Niagara = df.notl$Load, 
                           Eighteenmile = df.eighteen$Load, OakOrchard = oakorchard$Load,
                           Genesee = df.genesee$Load, Oswego = df.oswego$Load)
head(df.scenario5)

# convert units from g/s to kg/s
df.long <- df.scenario5 %>% pivot_longer(cols = -Date,
                                         names_to = "River",
                                         values_to = "TP_gs")
df.long$TP_kgs <- df.long$TP_gs / 1000
df.long2 <- subset(df.long, select = -TP_gs)

df2.scenario5 <- df.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
head(df2.scenario5)

write.csv(df2.scenario5, output_file)

################################################################################
#EFDC TP 2018: LOTP_EFDC_2018_input_scenario5.csv 
################################################################################
output_file <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/LOTP_EFDC_2018_input_scenario5.csv"

niagara <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Niagara_River_2018.txt"
eighteen <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/EighteenMile_River_2018.txt"
genesee <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Genessee_River_2018.txt"
oak <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Oakorchard_River_2018.txt"
oswego <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Oswego_River_2018.txt"
black <- "C:/Users/jmaure01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/research/LOTP/input data/2018/textfiles_atmos/Black_River_2018.txt"

#Niagara - NOTL-LDT
niagara <- read.table(niagara,
                      header = FALSE)
names(niagara) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
niagara$dateTime <- as.Date(with(niagara, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.notl <- data.frame(siteName = "Niagara", 
                      dateTime = niagara$dateTime, 
                      flow = niagara$Flow_cfs*0.0283, 
                      TP = niagara$TP_mgL)
df.notl$Load <- df.notl$flow*df.notl$TP
head(df.notl)

# Eighteenmile
eighteen <- read.table(file = eighteen,
                       header = FALSE)
names(eighteen) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
eighteen$dateTime <- as.Date(with(eighteen, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.eighteen <- data.frame(siteName = "Eighteenmile", dateTime = eighteen$dateTime, 
                          flow = (eighteen$Flow_cfs*0.0283), TP = eighteen$TP_mgL)
df.eighteen$Load <- df.eighteen$flow * df.eighteen$TP
head(df.eighteen)

#OakOrchard
oak <- read.table(file = oak,
                  header = FALSE)
names(oak) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oak$dateTime <- as.Date(with(oak, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

oakorchard <- data.frame(River = "OakOrchard", dateTime = oak$dateTime, 
                         flow = oak$Flow_cfs*0.0283, TP = oak$TP_mgL)
oakorchard$Load <- oakorchard$flow * oakorchard$TP
head(oakorchard)

#Genesee - 50% reduction in concentration
genesee <- read.table(file = genesee,
                      header = FALSE)
names(genesee) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
genesee$dateTime <- as.Date(with(genesee, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.genesee <- data.frame(River = "Genesee", dateTime = genesee$dateTime, 
                         flow = genesee$Flow_cfs*0.0283, TP = genesee$TP_mgL/2)
df.genesee$Load <- df.genesee$flow*df.genesee$TP
head(df.genesee)

#Oswego
oswego <- read.table(file = oswego,
                     header = FALSE)
names(oswego) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
oswego$dateTime <- as.Date(with(oswego, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.oswego <- data.frame(River = "Oswego", dateTime = oswego$dateTime, 
                        flow = oswego$Flow_cfs*0.0283, TP = oswego$TP_mgL)
df.oswego$Load <- df.oswego$flow * df.oswego$TP
head(df.oswego)

#Black
black <- read.table(file = black,
                    header = FALSE)
names(black) <- c("Month", "Day", "Year", "Flow_cfs", "TP_mgL", "Load_mtd")
black$dateTime <- as.Date(with(black, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df.black <- data.frame(River = "Black", dateTime = black$dateTime,
                       flow = black$Flow_cfs*0.0283, TP = black$TP_mgL,
                       Load_mtd = black$Load_mtd)
#convert load from mt/day to g/s
df.black$Load <- df.black$Load_mtd/(60*60*24) * 1e06

head(df.black)


df.scenario5 <- data.frame(Date = df.notl$dateTime, Niagara = df.notl$Load, 
                           Eighteenmile = df.eighteen$Load, OakOrchard = oakorchard$Load,
                           Genesee = df.genesee$Load, Oswego = df.oswego$Load, Black = df.black$Load)
head(df.scenario5)

# convert units from g/s to kg/s
df.long <- df.scenario5 %>% pivot_longer(cols = -Date,
                                         names_to = "River",
                                         values_to = "TP_gs")
df.long$TP_kgs <- df.long$TP_gs / 1000
df.long2 <- subset(df.long, select = -TP_gs)

df2.scenario5 <- df.long2 %>% pivot_wider(names_from = "River", values_from = "TP_kgs")
head(df2.scenario5)

write.csv(df2.scenario5, output_file)


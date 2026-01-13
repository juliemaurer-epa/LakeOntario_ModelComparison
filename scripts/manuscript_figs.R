# Manuscript figures
library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot)
library(ggpattern)

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/")

##### figure 3 - observational data ######
#combined data
df.tp <- read.csv("MvM_TP_combined.csv", header = TRUE) %>%
  mutate(date_time = as.Date(date_time, format = "%Y-%m-%d"),
         depth_interval = factor(depth_interval, levels = c("<5m", "5-30m", ">30m")))

df.temp <- readRDS("../../TPData_processing/Temp_data_combined.RData") %>%
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y")) %>%
  dplyr::select(-c(X, X.1)) 

datemin <- as.POSIXct("2018-04-01")
datemax <- as.POSIXct("2018-10-01")

df.temp <- df.temp[df.temp$date_time >= datemin & df.temp$date_time <= datemax,]

pobsA <- ggplot(data = df.tp, aes(x = date_time, y = Real_TP)) + geom_point(alpha = 0.5) +
  labs(x = "", y = "TP (µg L-1)") + theme_classic() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"))
pobsA

pobsB <- ggplot(data = df.temp, aes(x = date_time, y = temp)) + geom_point(alpha = 0.5) +
  labs(x = "Date", y = "Temperature °C") + theme_classic() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"))
pobsB

fig3 <- plot_grid(pobsA, pobsB, align = "v", nrow = 2, ncol = 1)
fig3

##### figure 4 - surface temp timeseries #####

#### panel A - volume weighted surface temp
df1 <- read.csv("EFDC_VolWeighted_Temp.csv")
df2 <- read.csv("FVCOM_VolWeighted_Temp.csv")

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

pA <- gg4 + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
pA

pA.grey <- gg4 + scale_color_grey() + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
pA.grey

#### panel B - buoy data
B.45012 <- df.temp[df.temp$siteID == "45012",]

df.fvcom <- read.csv("fvcom_buoy_out.csv") %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "", tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                 "%Y-%m-%d"))
  )

df.efdc <- read.csv("efdc_buoy_out.csv") %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "", tryFormats = c("%Y-%m-%d %H:%M:%OS", 
                                                                 "%Y-%m-%d"))
  )

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

pB <- p3 + theme(axis.title.y = element_blank(),
                 axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

pB.grey <- p3 + scale_color_grey() + 
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
pB.grey

##### panel C - residuals
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
ggsave(plot = pC, filename = "~/plots/fig4_C.png", width = 10, height = 6)

pC2 <- pC + scale_color_grey()
pC2
ggsave(plot = pC2, filename = "~/plots/fig4_C_grayscale.png", width = 10, height = 6)

# combine temp over time figures

plot_grid(pA, pB, align = "h", nrow = 1, ncol = 2)
ggsave("~/plots/fig4_A_B.png", width = 10, height = 6)

plot_grid(pA.grey, pB.grey, align = "h", nrow = 1, ncol = 2)
ggsave("~/plots/fig4_A_B_greyscale.png", width = 10, height = 6)

##### figure 5 - depth profile #####
## ON55M  
on55m <- subset(df.temp, siteID == "ON55M")
site <- "ON55M"
sitedepth <- unique(on55m$siteDepth)
p.title <- paste(site, "(43.44, -77.44)", sep = " ")

# FVCOM
fvcom.site <- readRDS("ON55M_fvcom.RData")

midpoint <- min(fvcom.site$temp) + (max(fvcom.site$temp) - min(fvcom.site$temp))/2

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

#greyscale
p_hm1 <- ggplot() +
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = on55m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  geom_point(data = on55m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

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

# EFDC
efdc.site <- readRDS("ON55M_efdc.RData")

model <- "EFDC"
fill.title <- paste(model, "Temp °C", sep = " ")

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

#greyscale
p_hm1 <- ggplot() +
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = on55m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  geom_point(data = on55m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

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

## ON33M 
on33m <- subset(df.temp, siteID == "ON33M")
site <- "ON33M"
p.title <- paste(site, "(43.60, -78.81)", sep = " ")

fvcom.site <- readRDS("ON33M_fvcom.RData")

midpoint <- min(fvcom.site$temp) + (max(fvcom.site$temp) - min(fvcom.site$temp))/2

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
p_hm3 # save as pdf to edit in illustrator

#greyscale
p_hm1 <- ggplot() +
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = on33m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  geom_point(data = on33m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

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

# EFDC
efdc.site <- readRDS("ON33M_efdc.RData")

model <- "EFDC"
fill.title <- paste(model, "Temp °C", sep = " ")

custom.blue <- "#56B4E9"

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
p_hm3 # save as pdf to edit in illustrator

#greyscale
p_hm1 <- ggplot() +
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = on33m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  geom_point(data = on33m, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

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

## NRM25
nrm25 <- subset(df.temp, siteID == "NRM25")
site <- "NRM25"
p.title <- paste(site, "(43.32, -79.04)", sep = " ")

fvcom.site <- readRDS("NRM25_fvcom.RData")

midpoint <- min(fvcom.site$temp) + (max(fvcom.site$temp) - min(fvcom.site$temp))/2

custom.blue <- "#56B4E9"

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
p_hm3 # save as pdf to edit in illustrator

#greyscale
p_hm1 <- ggplot() +
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = nrm25, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  geom_point(data = nrm25, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

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

# EFDC
efdc.site <- readRDS("NRM25_efdc.RData")
model <- "EFDC"
fill.title <- paste(model, "Temp °C", sep = " ")

custom.blue <- "#56B4E9"

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
p_hm3 # save as pdf to edit in illustrator

#greyscale
p_hm1 <- ggplot() +
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = nrm25, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  geom_point(data = nrm25, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

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

#### GRM40 
grm40 <- subset(df.temp, siteID == "GRM40")
site <- "GRM40"
p.title <- paste(site, "(43.34, -77.57)", sep = " ")

fvcom.site <- readRDS("GRM40_fvcom.RData")

midpoint <- min(fvcom.site$temp) + (max(fvcom.site$temp) - min(fvcom.site$temp))/2

custom.blue <- "#56B4E9"

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
p_hm3 # save as pdf to edit in illustrator

#greyscale
p_hm1 <- ggplot() +
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = grm40, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = fvcom.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  geom_point(data = grm40, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

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

# EFDC
efdc.site <- readRDS("GRM40_efdc.RData")
model <- "EFDC"
fill.title <- paste(model, "Temp °C", sep = " ")

custom.blue <- "#56B4E9"

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
p_hm3 # save as pdf to edit in illustrator

#greyscale
p_hm1 <- ggplot() +
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  theme_minimal()  + scale_y_reverse() +
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title)
p_hm1

# add data
p_hm2 <- ggplot() + geom_point(data = grm40, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(color = "Observed Temp °C") + theme_minimal() + scale_y_reverse()
p_hm2

p_hm0 <- ggplot() + 
  geom_tile(data = efdc.site, aes(x = dateTime, y = Depth, fill = temp)) +
  scale_fill_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint) +
  geom_point(data = grm40, aes(x = date_time, y = sampleDepth, color = temp), size = 2, shape = 16) + 
  scale_color_gradient2(low = "gray85", mid = "gray40", high = "gray10", midpoint = midpoint)  + 
  labs(x = "Time", y = "Depth (m)", fill = fill.title, title = p.title, color = "Observed Temp °C") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme_minimal() + scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        legend.position = "none")
p_hm0 

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

##### figure 6 - TP data and residuals #####
#panel A - boxplot
df.tp.l <- df.tp %>%
  pivot_longer(cols = c(13:21),
               names_to = "Model",
               values_to = "TP") %>%
  mutate(Model = case_when(Model == "Real_TP" ~ "TP Data",
                           Model == "FVCOM_scen1" ~ "FVCOM-LOTPM 1",
                           Model == "EFDC_scen1" ~ "EFDC-LOPTM 1",
                           Model == "LOEM_scen1" ~ "LOEM 1",
                           Model == "FVCOM_scen2" ~ "FVCOM-LOTPM 2",
                           Model == "EFDC_scen2" ~ "EFDC-LOTPM 2",
                           Model == "LOEM_scen2" ~ "LOEM 2",
                           Model == "EFDC_scen3" ~ "EFDC-LOPTM 3",
                           Model == "LOEM_scen3" ~ "LOEM 3")) %>%
  mutate(Model = factor(Model, levels = c("TP Data", "FVCOM-LOTPM 1", "EFDC-LOPTM 1",
                                          "LOEM 1", "FVCOM-LOTPM 2", "EFDC-LOTPM 2",
                                          "LOEM 2", "EFDC-LOPTM 3", "LOEM 3")))


colorvar1 <- c("#E69F00", "#56B4E9", "#009E73")

p5A <- ggplot(data = df.tp.l, aes(x = TP, y = Model, fill = depth_interval)) + 
  geom_boxplot(position = "dodge", outliers = FALSE) + theme_classic() +
  scale_fill_manual(values = colorvar1) + 
  labs(x = "Total Phosphorus (µg L-1)", y = "", fill = "Depth Interval") +
  labs(x = "Total Phosphorus (µg L-1)", y = "Depth Interval", fill = "") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.position = "bottom")
p5A

p5A.gray <- ggplot(data = df.tp.l, aes(x = TP, y = Model, fill = depth_interval)) + 
  geom_boxplot(position = "dodge", outliers = FALSE) + theme_classic() +
  scale_fill_grey() + 
  labs(x = "Total Phosphorus (µg L-1)", y = "", fill = "Depth Interval") +
  labs(x = "Total Phosphorus (µg L-1)", y = "Depth Interval", fill = "") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.position = "bottom")
p5A.gray

#panel B - residuals
df.res <- df.tp %>%
  mutate(res_fvcom1 = FVCOM_scen1 - Real_TP,
         res_efdc1 = EFDC_scen1 - Real_TP,
         res_loem1 = LOEM_scen1 - Real_TP,
         res_fvcom2 = FVCOM_scen2 - Real_TP,
         res_efdc2 = EFDC_scen2 - Real_TP,
         res_loem2 = LOEM_scen2 - Real_TP,
         res_efdc3 = EFDC_scen3 - Real_TP,
         res_loem3 = LOEM_scen3 - Real_TP) %>% 
  select(-c(FVCOM_scen1, EFDC_scen1, LOEM_scen1, FVCOM_scen2, EFDC_scen2, 
            LOEM_scen2, EFDC_scen3, LOEM_scen3)) %>%
  pivot_longer(cols = -c(1:14),
               names_to = "Model",
               values_to = "residuals") %>%
  mutate(Model = factor(Model, levels = c("res_efdc1", "res_loem1", "res_fvcom1", 
                                          "res_efdc2", "res_loem2", "res_fvcom2",
                                          "res_efdc3", "res_loem3")))

colorvar1 <- c("#E69F00", "#56B4E9", "#009E73")

model_names <- c(
  "res_efdc1" = "EFDC-LOTPM Scenario1",
  "res_loem1" = "LOEM Scenario1",
  "res_fvcom1" = "FVCOM-LOTPM Scenario1",
  "res_efdc2" = "EFDC-LOTPM Scenario2",
  "res_loem2" = "LOEM Scenario2",
  "res_fvcom2" = "FVCOM-LOTPM Scenario2",
  "res_efdc3" = "EFDC-LOTMP Scenario3",
  "res_loem3" = "LOEM Scenario3")

pris <- ggplot(df.res, aes(x = log(Real_TP), y = log(residuals), color = depth_interval)) + 
  geom_point(alpha = 0.75) + theme_classic() + facet_wrap(~Model, scales = "free_x", labeller = labeller(Model = model_names)) + 
  geom_abline(intercept = 0, slope = 0, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar1) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(-6,5)) +
  labs(x = "log(Measured TP)", y = "log(Modeled - Measured)", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12, color = "black"),
        legend.position = "bottom")
pris
#grayscale
pris2 <- pris + scale_color_grey()
pris2


## FVCOM vs EFDC vs LOEM TP analysis
# Author: Julie Maurer
# Created: 10/29/25

library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot) #easy multipanel plots

setwd("/work/GLHABS/GreatLakesEco/LakeOntario/ModelComparison/Statistics/")

#combined data
df <- read.csv("MvM_TP_combined.csv", header = TRUE) %>%
  mutate(date_time = as.Date(date_time, format = "%Y-%m-%d"),
         depth_interval = factor(depth_interval, levels = c("<5m", "5-30m", ">30m")))

#### observations over time and depth ####
pobsA <- ggplot(data = df, aes(x = date_time, y = Real_TP)) + geom_point(alpha = 0.5) +
  labs(x = "Date", y = "TP (µg L-1)") + theme_classic() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%b") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"))
pobsA

pobsB <- ggplot(data = df, aes(x = depth_interval)) + geom_bar(fill = "black") + 
  labs(x = "Depth Intervals", y = "Number of Observations") + theme_classic() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"))
pobsB

plot_grid(pobsA, pobsB, align = "h", nrow = 1, ncol = 2)
ggsave("../plots/TPData_2panel.png", width = 10, height = 10)

#### linear regression plots ####
colorvar1 <- c("#E69F00", "#56B4E9", "#009E73")

df2 <- df %>%
  mutate(log_TP = log(Real_TP),
         log_FVCOM1 = log(FVCOM_scen1),
         log_EFDC1 = log(EFDC_scen1),
         log_LOEM1 = log(LOEM_scen1),
         log_FVCOM2 = log(FVCOM_scen2),
         log_EFDC2 = log(EFDC_scen2),
         log_LOEM2 = log(LOEM_scen2),
         log_EFDC3 = log(EFDC_scen3),
         log_LOEM3 = log(LOEM_scen3))

#fvcom 1
f1 <- ggplot(data = df2, aes(x = log_TP, y = log_FVCOM1, color = depth_interval)) + theme_classic() +
  geom_point() + geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(0,5)) +
  labs(x = "", y = "Modeled TP (log10)", title = "FVCOM-LOTPM Scenario 1", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.position = "none")
#ggsave(filename = "../plots/MvM_TP_FVCOM_scen1.png", width = 7, height = 7)

#efdc 1
e1 <- ggplot(data = df2, aes(x = log_TP, y = log_EFDC1, color = depth_interval)) + theme_classic() +
  geom_point() + geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(0,5)) +
  labs(x = "", y = "", title = "EFDC-LOTPM Scenario 1", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.position = "none")
#ggsave(filename = "../plots/MvM_TP_EFDC_scen1.png", width = 7, height = 7)

#loem 1
l1 <- ggplot(data = df2, aes(x = log_TP, y = log_LOEM1, color = depth_interval)) + theme_classic() +
  geom_point() + geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(0,5)) +
  labs(x = "", y = "", title = "LOEM Scenario 1", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.position = "none")
#ggsave(filename = "../plots/MvM_TP_LOEM_scen1.png", width = 7, height = 7)

#fvcom 2
f2 <- ggplot(data = df2, aes(x = log_TP, y = log_FVCOM2, color = depth_interval)) + theme_classic() +
  geom_point() + geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(0,5)) +
  labs(x = "", y = "", title = "FVCOM-LOTPM Scenario 2", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.position = "none")
#ggsave(filename = "../plots/MvM_TP_FVCOM_scen2.png", width = 7, height = 7)

#efdc 2
e2 <- ggplot(data = df2, aes(x = log_TP, y = log_EFDC2, color = depth_interval)) + theme_classic() +
  geom_point() + geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(0,5)) +
  labs(x = "Measured TP (log10)", y = "", title = "EFDC-LOTPM Scenario 2", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.position = "bottom")
#ggsave(filename = "../plots/MvM_TP_EFDC_scen2.png", width = 7, height = 7)

#loem 2
l2 <- ggplot(data = df2, aes(x = log_TP, y = log_LOEM2, color = depth_interval)) + theme_classic() +
  geom_point() + geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(0,5)) +
  labs(x = "", y = "", title = "LOEM Scenario 2", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.position = "none")
#ggsave(filename = "../plots/MvM_TP_LOEM_scen2.png", width = 7, height = 7)

#efdc 3
ggplot(data = df2, aes(x = log_TP, y = log_EFDC3, color = depth_interval)) + theme_classic() +
  geom_point() + geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(0,5)) +
  labs(x = "Measured TP (log10)", y = "Modeled TP (log10)", title = "EFDC-LOTPM Scenario 3", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"))
#ggsave(filename = "../plots/MvM_TP_EFDC_scen3.png", width = 7, height = 7)

#loem 3
ggplot(data = df2, aes(x = log_TP, y = log_LOEM3, color = depth_interval)) + theme_classic() +
  geom_point() + geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(0,5)) +
  labs(x = "Measured TP (log10)", y = "Modeled TP (log10)", title = "LOEM Scenario 3", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"))
#ggsave(filename = "../plots/MvM_TP_LOEM_scen3.png", width = 7, height = 7)

# cols = model, rows = scenario

plot_grid(f1, e1, l1, f2, e2, l2, align = "hv", nrow = 2, ncol = 3)
#aligns plots but cuts legend off for e2

# trying facetting 
dfl <- df2 %>% 
  select(-c(Real_TP, FVCOM_scen1, EFDC_scen1, LOEM_scen1, FVCOM_scen2, EFDC_scen2, 
            LOEM_scen2, EFDC_scen3, LOEM_scen3, log_EFDC3, log_LOEM3)) %>%
  pivot_longer(cols = -c(1:14),
               names_to = "Model",
               values_to = "modeled_TP") %>%
  mutate(Model = factor(Model, levels = c("log_FVCOM1", "log_EFDC1", "log_LOEM1",
                                          "log_FVCOM2", "log_EFDC2", "log_LOEM2")))

#labeling facet grid
model_names <- c("log_FVCOM1" = "FVCOM-LOTPM Scenario1",
                 "log_EFDC1" = "EFDC-LOTPM Scenario1",
                 "log_LOEM1" = "LOEM Scenario1",
                 "log_FVCOM2" = "FVCOM-LOTPM Scenario2",
                 "log_EFDC2" = "EFDC-LOTPM Scenario2",
                 "log_LOEM2" = "LOEM Scenario 2")

ggplot(dfl, aes(x = log_TP, y = modeled_TP, color = depth_interval)) + 
  geom_point() + theme_classic() + facet_wrap(~Model, scales = "free_x", labeller = labeller(Model = model_names)) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(0,5)) +
  labs(x = "Measured TP (log10)", y = "Modeled TP (log10)", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.position = "bottom")
ggsave("../plots/MvM_6panel.png", width = 10, height = 8)

#### residual plots ####
# residuals = mod val - obs val
df.res <- df2 %>%
  mutate(log_res_fvcom1 = log_FVCOM1 - log_TP,
         log_res_efdc1 = log_EFDC1 - log_TP,
         log_res_loem1 = log_LOEM1 - log_TP,
         log_res_fvcom2 = log_FVCOM2 - log_TP,
         log_res_efdc2 = log_EFDC2 - log_TP,
         log_res_loem2 = log_LOEM2 - log_TP,
         log_res_efdc3 = log_EFDC3 - log_TP,
         log_res_loem3 = log_LOEM3 - log_TP)

# trying plot - easier to interpret (above 0 is overestimate, under zero is underestimate)
colorvar1 <- c("#E69F00", "#56B4E9", "#009E73")

ggplot(df.res, aes(x = log_TP, y = log_res_fvcom1, color = depth_interval)) + theme_classic() +
  geom_point() + geom_abline(intercept = 0, slope = 0, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar1) + scale_x_continuous(limits = c(0,6)) + scale_y_continuous(limits = c(-6,6)) +
  labs(x = "log(Measured TP)", y = "log(Modeled) - log(Measured)", title = "FVCOM Scenario 1", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"))

# trying facetting 
df.res2 <- df.res %>% 
  dplyr::select(date_time, Longitude, Latitude, node_fvcom, sigma, node_EFDC, I_Index,
                J_Index, K_Index, sample_depth, water_depth, depth_interval, log_TP,
                log_res_fvcom1, log_res_efdc1, log_res_loem1, log_res_fvcom2, 
                log_res_efdc2, log_res_loem2, log_res_efdc3, log_res_loem3) %>%
  pivot_longer(cols = -c(1:13),
               names_to = "Model",
               values_to = "log_residuals") %>%
  mutate(Model = factor(Model, levels = c("log_res_efdc1", "log_res_loem1", "log_res_fvcom1", 
                                          "log_res_efdc2", "log_res_loem2", "log_res_fvcom2",
                                          "log_res_efdc3", "log_res_loem3")))

#labeling facet grid
model_names <- c(
                 "log_res_efdc1" = "EFDC-LOTPM Scenario 1",
                 "log_res_loem1" = "LOEM Scenario 1",
                 "log_res_fvcom1" = "FVCOM-LOTPM Scenario 1",
                 "log_res_efdc2" = "EFDC-LOTPM Scenario 2",
                 "log_res_loem2" = "LOEM Scenario 2",
                 "log_res_fvcom2" = "FVCOM-LOTPM Scenario 2",
                 "log_res_efdc3" = "EFDC-LOTMP Scenario 3",
                 "log_res_loem3" = "LOEM Scenario 3")

ggplot(df.res2, aes(x = log_TP, y = log_residuals, color = depth_interval)) + 
  geom_point() + theme_classic() + facet_wrap(~Model, scales = "free_x", labeller = labeller(Model = model_names)) + 
  geom_abline(intercept = 0, slope = 0, color = "black", linewidth = 0.75, linetype = "solid") +
  scale_color_manual(values = colorvar1) + scale_x_continuous(limits = c(0,5)) + scale_y_continuous(limits = c(-6,5)) +
  labs(x = "log(Measured TP)", y = "log(Modeled) - log(Measured)", color = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12, color = "black"),
        legend.position = "bottom")

ggsave("../plots/Residuals_8panel.png", width = 10, height = 10)
#### comparing median modeled vs measured TP ####

# requires long form with TP data included as a "model"
dfl2 <- df %>%
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


# trying model vs TP, colored by depth interval
colorvar1 <- c("#E69F00", "#56B4E9", "#009E73")

ggplot(data = dfl2, aes(x = TP, y = Model, fill = depth_interval)) + 
  geom_boxplot(position = "dodge", outliers = FALSE) + theme_classic() +
  scale_fill_manual(values = colorvar1) + 
  labs(x = "Total Phosphorus (µg L-1)", y = "", fill = "Depth Interval") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        title = element_text(size = 12, color = "black"),
        legend.position = "bottom")
ggsave(filename = "../plots/MvM_TP_box_depthintervals.png", width = 7, height = 8)

## supplemental figure 14 remake
efdc_og <- read.csv("Modeled-v-Measured_TP_2018_EFDC_LO2.csv") %>%
  mutate(date_time = as.Date(date_time, format = "%Y-%m-%d")) %>%
  rename("EFDC_original" = modelvalue,
         "Real_TP" = TP_ugL.1)

loem_og <- read.csv("Modeled-v-Measured_TP_2018_LOEM_original.csv") %>%
  mutate(date_time = as.Date(date_time, format = "%Y-%m-%d")) %>%
  rename("LOEM_original" = modelvalue,
         "Real_TP" = TP_ugL.1)

df2 <- df %>%
  left_join(efdc_og, by = c("date_time", "Longitude", "Latitude", "node_fvcom", "sigma", 
                            "node_EFDC", "I_Index", "J_Index", "K_Index", "sample_depth", "water_depth", "Real_TP")) %>%
  left_join(loem_og, by = c("date_time", "Longitude", "Latitude", "node_fvcom", "sigma", 
                          "node_EFDC", "I_Index", "J_Index", "K_Index", "sample_depth", "water_depth", "Real_TP")) %>%
  select(date_time, Longitude, Latitude, node_fvcom, sigma, node_EFDC, I_Index, 
         J_Index, K_Index, sample_depth, water_depth, depth_interval, Real_TP, EFDC_original, 
         LOEM_original, EFDC_scen1, LOEM_scen1, EFDC_scen2, LOEM_scen2, 
         EFDC_scen3, LOEM_scen3) %>%
  pivot_longer(cols = c(13:20),
               names_to = "Model",
               values_to = "TP") %>%
  mutate(Model = case_when(Model == "Real_TP" ~ "TP Data",
                           Model == "EFDC_original" ~ "EFDC-LOTPM 0",
                           Model == "LOEM_original" ~ "LOEM 0",
                           Model == "EFDC_scen1" ~ "EFDC-LOPTM 1",
                           Model == "LOEM_scen1" ~ "LOEM 1",
                           Model == "EFDC_scen2" ~ "EFDC-LOTPM 2",
                           Model == "LOEM_scen2" ~ "LOEM 2",
                           Model == "EFDC_scen3" ~ "EFDC-LOPTM 3",
                           Model == "LOEM_scen3" ~ "LOEM 3")) %>%
  mutate(Model = factor(Model, levels = c("TP Data", "EFDC-LOTPM 0", "LOEM 0", 
                                          "EFDC-LOPTM 1", "LOEM 1", "EFDC-LOTPM 2", "LOEM 2", 
                                          "EFDC-LOPTM 3", "LOEM 3")))
  

colorvar1 <- c("#E69F00", "#56B4E9", "#009E73")

ggplot(data = df2, aes(x = TP, y = Model, fill = depth_interval)) + 
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
ggsave(filename = "../plots/supplemental15.png", width = 7, height = 8)


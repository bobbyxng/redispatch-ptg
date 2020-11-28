############################################
### SESAM Master thesis -- Plotting tool ###
############################################

# Authors: Bobby Xiong, Johannes Predel
# (C) 2020

#######################
### Input settings ####
#######################

session <- c("2020-06-17__13-17-28__ptdf__ieee_rts_24__1-168")

input_data <- "elmod_cal_exchange"

#############
### SETUP ###
#############

### Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(extrafont)

### Directory
session_dir <- paste("output/", session, "/", sep = "")
if (!dir.exists(paste(session_dir[1], "plots", sep = ""))) dir.create(paste(session_dir[1], "plots", sep = ""))
if (!dir.exists(paste(session_dir[1], "plots/png", sep = ""))) dir.create(paste(session_dir[1], "plots/png", sep = ""))

input_data_dir <- paste("data/", input_data, "/", sep = "")

source("R/util.R", echo = FALSE)        # Functions
source("R/fuel_setup_DE.R", echo = FALSE)  # Fuel settings
if (!exists("load_results")) source("R/load_results_DE.R", echo = TRUE)    # Load data from results in CSV

### Font (LaTeX)
font_import("R/fonts", prompt = FALSE)

#####################
### PLOT SETTINGS ###
#####################

# loadfonts(device = "pdf")
loadfonts(device = "win")

### Settings
# Legend parameters
plot_legend <- theme(legend.position = "bottom",
                     legend.box.margin = margin(-8,-8,-8,-8),
                     legend.key.size = unit(0.6,"line"))

# Theme parameters
plot_theme <- theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_line(colour = "black", size = 0.2)) +
  theme(text = element_text(face  = "plain", size = 8, family = "NimbusRomNo9L"))

# Width of the plot in cm
plot_width <- 12.6
plot_width_png <- 20

# Display every n-th step on the x axis
plot_x_step <- 12

# Line width
plot_linesize = 0.3

#############
### PLOTS ###
#############

###################
### MERIT ORDER ###
###################

p_merit_order <- ggplot(ED_merit_order, aes(ymin = 0)) + 
  geom_rect(aes(xmin = min_capacity/1000, xmax = cum_capacity/1000,
                ymax = mc, fill = fuel)) +
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend +
  xlab("Installed Capacity (GW)") + 
  ylab("Marginal cost (\u20ac/MWh)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, last(ED_merit_order$cum_capacity), by = plot_x_step)) +
  scale_y_continuous(expand = c(0, 0))

if (exists("p_merit_order")) {
  ggsave(filename = paste(session_dir, "plots/", "plot_merit_order.pdf", sep = ""), 
         plot = p_merit_order, device = "pdf", width = plot_width, height = 5.5, units = "cm", dpi = 150,
         limitsize = FALSE)
  
  ggsave(filename = paste(session_dir, "plots/png/", "plot_merit_order.png", sep = ""), 
         plot = p_merit_order, device = "png", width = plot_width_png, height = 5.5, units = "cm", dpi = 150)
}

#########################
### ECONOMIC DISPATCH ###
#########################

### Economic dispatch PLOT
p_economic_dispatch <- ggplot() + 
  geom_area(data = ED_dispatch %>% subset(time %in% hours_feasible), aes(x = time, y = output, fill = fuel), position = "stack") + 
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend +
  xlab("Hour (h)") + 
  ylab("Dispatch (MW)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, last(ED_dispatch$time), by = plot_x_step)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_area(data = ED_D_S, aes(x = time, y = output, fill = fuel), position = "stack") +
  geom_line(data = data_load_time, aes(x = time, y = load), size = plot_linesize)

if (exists("p_economic_dispatch")) {
  ggsave(filename = paste(session_dir[1], "plots/", "plot_economic_dispatch.pdf", sep = ""), 
         plot = p_economic_dispatch, device = "pdf", width = 1*plot_width, height = 5.5, units = "cm", dpi = 150,
         limitsize = FALSE)
  
  ggsave(filename = paste(session_dir[1], "plots/png/", "plot_economic_dispatch.png", sep = ""), 
         plot = p_economic_dispatch, device = "png", width = plot_width_png, height = 5.5, units = "cm", dpi = 150)
}

#############################
### MARKET CLEARING PRICE ### 
#############################

### Market clearing price PLOT
p_mcp <- ggplot(ED_price %>% subset(time %in% hours_feasible), aes(x = time, y = price)) + 
  geom_line(size = plot_linesize) + 
  plot_theme +
  xlab("Hour (h)") + 
  ylab("MCP (\u20ac/MWh)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, last(ED_price$time), by = plot_x_step)) +
  scale_y_continuous(expand = c(0, 0))

if (exists("p_mcp")) {
  ggsave(filename = paste(session_dir[1], "plots/", "plot_market_clearing_price.pdf", sep = ""), 
         plot = p_mcp, device = "pdf", width = 1*plot_width, height = 5.5, units = "cm", dpi = 150,
         limitsize = FALSE)
  
  ggsave(filename = paste(session_dir[1], "plots/png/", "plot_market_clearing_price.png", sep = ""), 
         plot = p_mcp, device = "png", width = plot_width_png, height = 5.5, units = "cm", dpi = 150)
}
  
#############################
### CONGESTION MANAGEMENT ###
#############################

### Congestion management PLOT
p_congestion_management <- ggplot(CM_redispatch %>% subset(time %in% hours_feasible), aes(x = time, y = output, fill = fuel)) + 
  geom_bar(stat = "identity", position = "stack", width = 1) + 
  geom_hline(yintercept = 0, size = 0.15) + 
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend +
  xlab("Hour (h)") + 
  ylab("Redispatch (MW)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, last(CM_redispatch$time), by = plot_x_step)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(nrow = 4,byrow = FALSE))

if (exists("p_congestion_management")) {
  ggsave(filename = paste(session_dir[1], "plots/", "plot_redispatch.pdf", sep = ""), 
         plot = p_congestion_management, device = "pdf", width = 1*plot_width, height = 5.5, units = "cm", dpi = 150, limitsize = FALSE)
  
  ggsave(filename = paste(session_dir[1], "plots/png/", "plot_redispatch.png", sep = ""), 
         plot = p_congestion_management, device = "png", width = plot_width_png, height = 5.5, units = "cm", dpi = 150)
}

###################################
### CONGESTION MANAGEMENT + PTG ###
###################################

p_congestion_management_ptg <- ggplot(CM_PtG_redispatch %>% subset(time %in% hours_feasible), aes(x = time, y = output, fill = fuel)) + 
  geom_bar(stat = "identity", position = "stack", width = 1) + 
  geom_hline(yintercept = 0, size = 0.15) + 
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) + 
  plot_legend + 
  xlab("Hour (h)") + 
  ylab("Redispatch (MW)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, last(CM_PtG_redispatch$time), by = plot_x_step)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(nrow = 4,byrow = FALSE))

if (exists("p_congestion_management_ptg")) {
  ggsave(filename = paste(session_dir[1], "plots/", "plot_redispatch_ptg.pdf", sep = ""), 
         plot = p_congestion_management_ptg, device = "pdf", width = 1*plot_width, height = 5.5, units = "cm", dpi = 150, limitsize = FALSE)
  
  ggsave(filename = paste(session_dir[1], "plots/png/", "plot_redispatch_ptg.png", sep = ""), 
         plot = p_congestion_management_ptg, device = "png", width = plot_width_png, height = 5.5, units = "cm", dpi = 150)
}

#################
### PIE CHART ###
#################

p_pie <- ggplot(data_pie, aes(x = 2899, y = output, fill = fuel)) +
  geom_bar(width = 1, stat = "identity", position = "stack") +
  coord_polar("y", start=0.5*pi) +
  theme_void() +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend  +
  geom_text(aes(x = 2900, label = label), family = "NimbusRomNo9L", size = 1.5, position = position_stack(vjust = 0.5), color = "black", na.rm = TRUE) +
  xlim(2896, 2900) +
  theme(text = element_text(face  = "plain", size = 8, family = "NimbusRomNo9L")) +
  guides(fill=guide_legend(nrow = 3,byrow = FALSE))

if (exists("p_pie")) {
  ggsave(filename = paste(session_dir[1], "plots/", "plot_economic_dispatch_pie.pdf", sep = ""), 
         plot = p_pie + facet_grid(cols = vars(source)), device = "pdf", width = plot_width, height = 5.5, units = "cm", dpi = 150,
         limitsize = FALSE)
  
  ggsave(filename = paste(session_dir[1], "plots/png/", "plot_economic_dispatch_pie.png", sep = ""), 
         plot = p_pie + facet_grid(cols = vars(source)), device = "png", width = plot_width_png, height = 5.5, units = "cm", dpi = 150)
}

###########################
### CM + PTG MECHANISMS ###
###########################

scale_ptg <- 10

### CM + PtG mechanisms PLOT 
p_mech_ptg <- ggplot(data_pmin_maxres %>% subset(time %in% hours_feasible), aes(x = time, y = output)) + 
  geom_area(position = "stack", aes(fill = fuel)) + 
  geom_point(data = CM_PtG_D_PtG_grouped[which(CM_PtG_D_PtG_grouped$output>0),] %>% subset(time %in% hours_feasible), aes(x = time, y = scale_ptg *output), color = fcolors["SNG"], size = 0.4, stroke = plot_linesize) + 
  geom_segment(data = CM_PtG_D_PtG_grouped[which(CM_PtG_D_PtG_grouped$output>0),] %>% subset(time %in% hours_feasible), aes(x = time, xend = time, y = 0, yend = scale_ptg *output), color = fcolors["SNG"], size = plot_linesize) +
  geom_line(data = data_load_time %>% subset(time %in% hours_feasible), aes(x = time, y = load), size = plot_linesize) +
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend +
  xlab("Hour (h)") + 
  ylab("Dispatch (MW)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, last(data_pmin_maxres$time), by = plot_x_step)) +
  scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~./scale_ptg , name = "PtG (MW)"))

# PtG storage level
p_ptg_storage <- ggplot(CM_PtG_L_syn %>% subset(time %in% hours_feasible), aes(x = time, y = storage)) + 
  geom_line(size = plot_linesize) + 
  plot_theme +
  xlab("Hour (h)") + 
  ylab(expression("SNG (MWh"["th"]*")")) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, last(ED_price$time), by = plot_x_step)) +
  scale_y_continuous(expand = c(0, 0))

p_mech_full <- ggarrange(p_mcp, p_mech_ptg, p_ptg_storage,
                         ncol = 1, nrow = 3, align = "v",
                         heights = c(6, 10, 5))

if (exists("p_mech_full")) {
  ggsave(filename = paste(session_dir[1], "plots/", "plot_mechanism_ptg.pdf", sep = ""), 
         plot = p_mech_full, device = "pdf", width = 1*plot_width, height = 9, units = "cm", dpi = 150, limitsize = FALSE)
  
  ggsave(filename = paste(session_dir[1], "plots/png/", "plot_mechanism_ptg.png", sep = ""), 
         plot = p_mech_full, device = "png", width = plot_width_png, height = 9, units = "cm", dpi = 150)
}

#########################
### REDISPATCH VOLUME ### (TWh)
#########################

p_redispatch_volume <- ggplot(filter(data_redispatch_volume, fuel != "Lost gen"), aes(x = model, y = output/1e6, fill = fuel)) +
  geom_bar(colour = "black", size = 0.05, stat="identity") + coord_flip() +
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend +
  xlab("Model") + 
  ylab("Volume (TWh)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(nrow = 3,byrow = FALSE))

if (exists("p_redispatch_volume")) {
  ggsave(filename = paste(session_dir[1], "plots/", "plot_redispatch_volume.pdf", sep = ""), 
         plot = p_redispatch_volume, device = "pdf", width = plot_width, height = 4, units = "cm", dpi = 150)
  
  ggsave(filename = paste(session_dir[1], "plots/png/", "plot_redispatch_volume.png", sep = ""), 
         plot = p_redispatch_volume, device = "png", width = plot_width_png, height = 4, units = "cm", dpi = 150)
}


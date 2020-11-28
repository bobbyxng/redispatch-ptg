############################################
### SESAM Master thesis -- Plotting tool ###
############################################

# Authors: Bobby Xiong, Johannes Predel
# (C) 2020

#######################
### Input settings ####
#######################

session <- c("2020-06-20__20-34-53__ptdf__elmod_cal_noexchange__1-8760_trm25_biomass0")

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
library(plotly)
library(htmlwidgets)

### Directory
session_dir <- paste("output/", session, "/", sep = "")
if (!dir.exists(paste(session_dir[1], "plots_paper", sep = ""))) dir.create(paste(session_dir[1], "plots_paper", sep = ""))
if (!dir.exists(paste(session_dir[1], "plots_paper/png", sep = ""))) dir.create(paste(session_dir[1], "plots_paper/png", sep = ""))

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
                     legend.box.margin = margin(-6,-8,0,-28),
                     legend.key.size = unit(0.6,"line"))

# Theme parameters
plot_theme <- theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_line(colour = "black", size = 0.2)) +
  theme(text = element_text(face  = "plain", size = 8, family = "NimbusRomNo9L")) +
  theme(plot.margin=unit(c(0.05,0.05,0.05,0.05),"cm")) +
  theme(legend.margin=margin(0,0,0,0),
        legend.justification="center",
        legend.spacing.x = unit(0.1, 'cm'))

# Width of the plot in cm
col_width <- 7.7
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
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(ncol = 5, byrow = FALSE)) 

if (exists("p_merit_order")) {
  ggsave(filename = paste(session_dir, "plots_paper/", "plot_merit_order.pdf", sep = ""), 
         plot = p_merit_order, device = "pdf", width = col_width, height = 4.7, units = "cm", dpi = 150,
         limitsize = FALSE)
  
  ggsave(filename = paste(session_dir, "plots_paper/png/", "plot_merit_order.png", sep = ""), 
         plot = p_merit_order, device = "png", width = plot_width_png, height = 5.5, units = "cm", dpi = 150)
}

#########################
### ECONOMIC DISPATCH ###
#########################

### Economic dispatch PLOT
p_economic_dispatch <- ggplot() + 
  geom_area(data = ED_dispatch_daily, aes(x = day, y = output/1e3, fill = fuel), position = "stack") + 
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend +
  xlab("Day") + 
  ylab("Dispatch (GWh)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_area(data = ED_D_S_daily, aes(x = day, y = output/1e3, fill = fuel), position = "stack") +
  geom_line(data = data_load_time_daily, aes(x = day, y = load/1e3, colour = "black"), size = plot_linesize) + 
  scale_colour_manual(values = "black", name = "", labels = "Load") +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE)) 

if (exists("p_economic_dispatch")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_economic_dispatch.pdf", sep = ""), 
         plot = p_economic_dispatch, device = "pdf", width = 1*plot_width, height = 6, units = "cm", dpi = 150,
         limitsize = FALSE)
}

#############################
### MARKET CLEARING PRICE ### 
#############################

### Market clearing price PLOT
p_mcp <- ggplot(ED_price_daily) + 
  geom_ribbon(aes(x = day, ymin = price_min, ymax = price_max, fill = "#dddddd")) +
  geom_line(aes(x = day, y = price_mean, colour = "black"), size = plot_linesize) + 
  xlab("Day") + 
  ylab("MP (\u20ac/MWh)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = "#dddddd", name = "", labels = "Range of market clearing price during the day") +
  scale_colour_manual(values = "black", name = "", labels = "Mean market clearing price") +
  plot_theme+
  plot_legend

if (exists("p_mcp")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_market_clearing_price.pdf", sep = ""), 
         plot = p_mcp, device = "pdf", width = plot_width, height = 5, units = "cm", dpi = 150,
         limitsize = FALSE)
}


################
### ED + MCP ###
################

p_ed_mcp <- ggarrange(p_mcp, p_economic_dispatch,
                         ncol = 1, nrow = 2, align = "v",
                         heights = c(6, 7))

if (exists("p_ed_mcp")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_economic_dispatch_mcp.pdf", sep = ""), 
         plot = p_ed_mcp, device = "pdf", width = plot_width, height = 9, units = "cm", dpi = 150,
         limitsize = FALSE)
}

  
#############################
### CONGESTION MANAGEMENT ###
#############################

### Congestion management PLOT
p_congestion_management <- ggplot(filter(CM_redispatch_daily, fuel != "Lost gen"), aes(x = day, y = output/1e3, fill = fuel)) + 
  geom_bar(stat = "identity", position = "stack", width = 1) + 
  geom_hline(yintercept = 0, size = 0.15) + 
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend +
  xlab("Day") + 
  ylab("Redispatch (GWh)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, last(CM_redispatch$time), by = plot_x_step)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(nrow = 2,byrow = FALSE)) +
  

if (exists("p_congestion_management")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_redispatch.pdf", sep = ""), 
         plot = p_congestion_management, device = "pdf", width = 1*plot_width, height = 5.5, units = "cm", dpi = 150, limitsize = FALSE)
  
  ggsave(filename = paste(session_dir[1], "plots_paper/png/", "plot_redispatch.png", sep = ""), 
         plot = p_congestion_management, device = "png", width = plot_width_png, height = 5.5, units = "cm", dpi = 150)
}

###################################
### CONGESTION MANAGEMENT + PTG ###
###################################

p_congestion_management_ptg <- ggplot(filter(CM_PtG_redispatch_daily, fuel != "Lost gen"), aes(x = day, y = output/1e3, fill = fuel)) + 
  geom_bar(stat = "identity", position = "stack", width = 1) + 
  geom_hline(yintercept = 0, size = 0.15) + 
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) + 
  plot_legend + 
  xlab("Day") + 
  ylab("Redispatch (GWh)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, last(CM_PtG_redispatch$time), by = plot_x_step)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(nrow = 2,byrow = FALSE))

if (exists("p_congestion_management_ptg")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_redispatch_ptg.pdf", sep = ""), 
         plot = p_congestion_management_ptg, device = "pdf", width = 1*plot_width, height = 5.5, units = "cm", dpi = 150, limitsize = FALSE)
}

#################
### PIE CHART ###
#################

p_pie <- ggplot(data_pie, aes(x = 7, y = output, fill = fuel)) +
  geom_bar(width = 0.7, stat = "identity", position = "stack") +
  coord_polar("y", start=1.0*pi) +
  theme_void() +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend  +
  geom_text(aes(x = 8, label = label), family = "NimbusRomNo9L", size = 2, position = position_stack(vjust = 0.5), color = "black", na.rm = TRUE) +
  geom_text(aes(x = 4, label = source_label), family = "NimbusRomNo9L", size = 2.5, position = position_stack(vjust = 0.5), color = "black", na.rm = TRUE) +
  xlim(4, 8) +
  theme(text = element_text(face  = "plain", size = 8, family = "NimbusRomNo9L")) +
  guides(fill=guide_legend(nrow = 2,byrow = FALSE)) +
  theme(legend.box.margin = margin(-12,-8,0,-28)) +
  theme(plot.margin = unit(c(-0.5,0,0,0), "cm"))

if (exists("p_pie")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_economic_dispatch_pie.pdf", sep = ""), 
         plot = p_pie + facet_grid(cols = vars(source)), device = "pdf", width = plot_width, height = 5.2, units = "cm", dpi = 150,
         limitsize = FALSE)
  
  ggsave(filename = paste(session_dir[1], "plots_paper/png/", "plot_economic_dispatch_pie.png", sep = ""), 
         plot = p_pie + facet_grid(cols = vars(source)), device = "png", width = plot_width_png, height = 5.5, units = "cm", dpi = 150)
}

# Pie alternative

p_generation_mix <- ggplot() +
  geom_bar(data = filter(data_pie), aes(x = source, y = output/1e6, fill = fuel), stat="identity") +
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend +
  xlab("Source") + 
  ylab("Volume (TWh)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), lim = c(0, 650))

ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_generation_mix.pdf", sep = ""), 
       plot = p_generation_mix, device = "pdf", width = col_width, height = 4.7, units = "cm", dpi = 150,
       limitsize = FALSE)


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
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_mechanism_ptg.pdf", sep = ""), 
         plot = p_mech_full, device = "pdf", width = 1*plot_width, height = 9, units = "cm", dpi = 150, limitsize = FALSE)
  
  ggsave(filename = paste(session_dir[1], "plots_paper/png/", "plot_mechanism_ptg.png", sep = ""), 
         plot = p_mech_full, device = "png", width = plot_width_png, height = 9, units = "cm", dpi = 150)
}

#########################
### REDISPATCH VOLUME ### (TWh)
#########################

p_redispatch_volume <- ggplot(filter(data_redispatch_volume, fuel != "Lost gen"), aes(x = model, y = output/1e6, fill = fuel)) +
  geom_bar(stat="identity") + coord_flip() +
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend +
  xlab("Model") + 
  ylab("Volume (TWh)") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(nrow = 2,byrow = FALSE))

if (exists("p_redispatch_volume")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_redispatch_volume.pdf", sep = ""), 
         plot = p_redispatch_volume, device = "pdf", width = plot_width, height = 3, units = "cm", dpi = 150)
  
  ggsave(filename = paste(session_dir[1], "plots_paper/png/", "plot_redispatch_volume.png", sep = ""), 
         plot = p_redispatch_volume, device = "png", width = plot_width_png, height = 4, units = "cm", dpi = 150)
}

#######################
### ED price spread ###
#######################

p_mcp_boxplot <- ggplot(ED_price_hourly, aes(x = factor(quarter), y = price, fill = season)) +
  geom_boxplot(fatten = 1.5, outlier.size = plot_linesize, outlier.stroke = 0, lwd = plot_linesize) +
  stat_summary(fun = mean, geom="point", aes(shape = "mean"), size=3, color="black", fill="black") +
  plot_theme +
  xlab("Hours") + 
  ylab("MP (\u20ac/MWh)") +
  scale_fill_manual(values = c(fcolors["SolarPV"][[1]], fcolors["WindOnshore"][[1]]), name = "", labels = c("Summer", "Winter")) + 
  scale_shape_manual(values = c("mean" = "x"), name ="", label = "Mean MCP") +
  scale_x_discrete(labels= c("1 - 2190", "2191 - 4380", "4381 - 6570", "6571 - 8760")) +
  plot_legend


g <- ggplotly(p_mcp_boxplot)
saveWidgetFix(g, paste(session_dir[1], "plots_paper/", "plot_mcp_boxplot.html", sep = ""))

if (exists("p_mcp_boxplot")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_mcp_boxplot.pdf", sep = ""), 
         plot = p_mcp_boxplot, device = "pdf", width = col_width, height = 4, units = "cm", dpi = 150)
  
  ggsave(filename = paste(session_dir[1], "plots_paper/png/", "plot_mcp_boxplot.pdf.png", sep = ""), 
         plot = p_mcp_boxplot, device = "png", width = col_width, height = 45, units = "cm", dpi = 150)
}

####################

CM_redispatch_daily$model <- "CM"
CM_PtG_redispatch_daily$model <- "CM + PtG"

CM_redispatch_combined <- rbind(CM_redispatch_daily, CM_PtG_redispatch_daily)
CM_redispatch_combined$fuel <-factor(CM_redispatch_combined$fuel, levels = fuel_levels_ext_lost)

p_congestion_management_combined <- ggplot(filter(CM_redispatch_combined, fuel != "Lost gen"), aes(x = day, y = output/1e3, fill = fuel)) + 
  geom_bar(stat = "identity", position = "stack", width = 1) + 
  geom_hline(yintercept = 0, size = 0.15) + 
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) + 
  plot_legend + 
  xlab("Day") + 
  ylab("Redispatch (GWh)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, last(CM_redispatch_combined$day), by = plot_x_step)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(nrow = 2,byrow = FALSE))

p_congestion_management_combined_facet <- p_congestion_management_combined + facet_grid(rows = vars(model)) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())

if (exists("p_congestion_management_combined")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_congestion_management_combined.pdf", sep = ""), 
         plot = p_congestion_management_combined_facet, device = "pdf", width = plot_width_png, height = 11, units = "cm", dpi = 150)
}



p_congestion_management_combined <- ggarrange(p_congestion_management, p_congestion_management_ptg,
                                              ncol = 1, nrow = 2, align = "v",
                                              heights = c(1, 1),
                                              common.legend = TRUE,
                                              legend = "bottom")

if (exists("p_congestion_management_combined")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_congestion_management_combined.pdf", sep = ""), 
         plot = p_congestion_management_combined, device = "pdf", width = plot_width, height = 10, units = "cm", dpi = 150,
         limitsize = FALSE)
}



p_congestion_management_flip <-  p_congestion_management + coord_flip() +
  scale_y_continuous(position = "right") +
  scale_x_reverse() +
  guides(fill=guide_legend(ncol = 5, byrow = FALSE)) 

ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_test.pdf", sep = ""), 
       plot = p_congestion_management_flip, device = "pdf", width = col_width, height = 20, units = "cm", dpi = 150)





##########################################
### ECONOMIC DISPATCH WEEKS BY QUARTER ###
##########################################
days_selected <- c(1:7,
                   134:140,
                   246:252,
                   358:364)


### Economic dispatch PLOT
p_economic_dispatch_quarter <- ggplot() + 
  geom_area(data = ED_dispatch_hourly %>% subset(day %in% days_selected), aes(x = time, y = output/1e3, fill = fuel), position = "stack") + 
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) +
  plot_legend +
  xlab("Hour") + 
  ylab("Dispatch (GWh)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(from = 1, to = 8760, by = 24)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_area(data = ED_D_S_hourly %>% subset(day %in% days_selected), aes(x = time, y = output/1e3, fill = fuel), position = "stack") +
  geom_line(data = data_load_time_hourly %>% subset(day %in% days_selected), aes(x = time, y = load/1e3, colour = "black"), size = plot_linesize) + 
  scale_colour_manual(values = "black", name = "", labels = "Load") +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE)) + 
  facet_wrap(.~quarter, nrow = 2, scale = "free_x") + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank())

if (exists("p_economic_dispatch_quarter")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_economic_dispatch_quarter.pdf", sep = ""), 
         plot = p_economic_dispatch_quarter, device = "pdf", width = 1*plot_width, height = 8, units = "cm", dpi = 150,
         limitsize = FALSE)
}

##############################################
### CONGESTION MANAGEMENT + PTG BY QUARTER ###
##############################################

p_congestion_management_ptg_quarter <- ggplot(filter(CM_PtG_redispatch_hourly, fuel != "Lost gen")%>% subset(day %in% days_selected), aes(x = time, y = output/1e3, fill = fuel)) + 
  geom_bar(stat = "identity", position = "stack", width = 1) + 
  geom_hline(yintercept = 0, size = 0.15) + 
  plot_theme +
  scale_fill_manual(values = fcolors, name = "", labels = flabels) + 
  plot_legend + 
  xlab("Hour") + 
  ylab("Redispatch (GWh)") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(from = 1, to = 8760, by = 24)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(nrow = 2,byrow = FALSE)) + 
  facet_wrap(.~quarter, nrow = 2, scale = "free_x") + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank())

if (exists("p_congestion_management_ptg_quarter")) {
  ggsave(filename = paste(session_dir[1], "plots_paper/", "plot_redispatch_ptg_quarter.pdf", sep = ""), 
         plot = p_congestion_management_ptg_quarter, device = "pdf", width = 1*plot_width, height = 8, units = "cm", dpi = 150, limitsize = FALSE)
}


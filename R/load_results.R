########################################
### SESAM Master thesis -- Data load ###
########################################

# Authors: Bobby Xiong, Johannes Predel
# (C) 2020

# Functions
source("R/util.R", echo = FALSE)

##################
### INPUT DATA ###
##################

# Nodes
data_nodes <- read.csv(file = paste(input_data_dir, "nodes.csv", sep = ""))
date_tool <- read.csv("R/date_tool.csv")

# Lines
data_lines <- read.csv(file = paste(input_data_dir, "lines.csv", sep = ""))
data_lines$pmax_circuits <- data_lines$circuits * data_lines$pmax

# Historic redispatch
data_bnetza_redipatch_cost <- read.csv(file = "data/redispatch_cost.csv")
data_bnetza_redipatch_volume <- read.csv(file = "data/redispatch_volume.csv")


##################
### MODEL INFO ###
##################

# Read info files
if (!exists("ED_info")) ED_info <- multiCSVtoDF("ED_info.csv")
if (!exists("CM_info")) CM_info <- multiCSVtoDF("CM_info.csv") 
if (!exists("CM_PtG_info")) CM_PtG_info <- multiCSVtoDF("CM_PtG_info.csv") 


# Set of feasible hours
ED_hours_feasible <- integer()
for (timeperiod in ED_info$timeperiod[ED_info$termination_status == "OPTIMAL"]) {
  if (!grepl(":", timeperiod, fixed = TRUE)) {
    hours <- as.numeric(timeperiod)
  } else {
    hour1 <- strsplit(timeperiod, split =":")[[1]][1] %>% as.integer()
    hour2 <- strsplit(timeperiod, split =":")[[1]][2] %>% as.integer()
    hours <- hour1:hour2
  }
  
  ED_hours_feasible <- append(ED_hours_feasible, hours)
}

CM_hours_feasible <- integer()
for (timeperiod in CM_info$timeperiod[CM_info$termination_status == "OPTIMAL"]) {
  if (!grepl(":", timeperiod, fixed = TRUE)) {
    hours <- as.numeric(timeperiod)
  } else {
    hour1 <- strsplit(timeperiod, split =":")[[1]][1] %>% as.integer()
    hour2 <- strsplit(timeperiod, split =":")[[1]][2] %>% as.integer()
    hours <- hour1:hour2
  }
  
  CM_hours_feasible <- append(CM_hours_feasible, hours)
}

CM_PtG_hours_feasible <- integer()
for (timeperiod in CM_PtG_info$timeperiod[CM_PtG_info$termination_status == "OPTIMAL"]) {
  if (!grepl(":", timeperiod, fixed = TRUE)) {
    hours <- as.numeric(timeperiod)
  } else {
    hour1 <- strsplit(timeperiod, split =":")[[1]][1] %>% as.integer()
    hour2 <- strsplit(timeperiod, split =":")[[1]][2] %>% as.integer()
    hours <- hour1:hour2
  }
  
  CM_PtG_hours_feasible <- append(CM_PtG_hours_feasible, hours)
}


hours_feasible <- intersect(ED_hours_feasible, intersect(CM_hours_feasible, CM_PtG_hours_feasible))

###################
### MERIT ORDER ###
###################

### Merit order DATA
data_merit_order_pp <- read.csv(file = paste(session_dir[1], "ED_merit_order_pp.csv", sep = ""))
data_merit_order_res <- read.csv(file = paste(session_dir[1], "ED_merit_order_res.csv", sep = ""))
data_merit_order_storages <- read.csv(file = paste(session_dir[1], "ED_merit_order_storages.csv", sep = ""))

ED_merit_order <- rbind(data_merit_order_pp, 
                        data_merit_order_res, 
                        data_merit_order_storages) 

ED_merit_order$fuel <- factor(ED_merit_order$fuel, levels = fuel_levels)

# Replace all mc = 0 with an arbitrary value for visual purposess
ED_merit_order$mc[ED_merit_order$fuel == "WindOnshore"] <- 1.499999991
ED_merit_order$mc[ED_merit_order$fuel == "WindOffshore"] <- 1.499999992
ED_merit_order$mc[ED_merit_order$fuel == "SolarPV"] <- 1.499999993
ED_merit_order$mc[ED_merit_order$fuel == "Biomass"] <- 1.499999994
ED_merit_order$mc[ED_merit_order$fuel == "RoR"] <- 1.499999995
ED_merit_order$mc[ED_merit_order$fuel == "Hydro"] <- 1.499999996
ED_merit_order$mc[ED_merit_order$fuel == "Geothermal"] <- 1.499999997
ED_merit_order$mc[ED_merit_order$fuel == "Waste"] <- 1.499999998


ED_merit_order <- ED_merit_order[order(ED_merit_order$mc),] 
ED_merit_order <- ED_merit_order %>% mutate(cum_capacity = cumsum(capacity))
ED_merit_order <- ED_merit_order %>% mutate(min_capacity = cum_capacity - capacity)

#########################
### ECONOMIC DISPATCH ###
#########################

# Load
if (!exists("data_load_time")) data_load_time <- multiCSVtoDF("load.csv")

data_load_time <- data_load_time %>% group_by(time) %>%
  summarise(load = sum(load))

data_load_time_hourly <- merge(data_load_time, date_tool, by.x = "time", by.y = "hour", all.x = TRUE)

data_load_time_daily <- data_load_time_hourly %>%
  group_by(day, quarter, weekday_number) %>%
  summarise(load = sum(load))

# Total dispatch 
if (!exists("ED_P")) ED_P <- multiCSVtoDF("ED_P.csv")         # Generation output of dispatchable power plants
if (!exists("ED_P_R")) ED_P_R <- multiCSVtoDF("ED_P_R.csv")   # Generation output of renewable energy units
if (!exists("ED_P_S")) ED_P_S <- multiCSVtoDF("ED_P_S.csv")   # Generation output of storages units

ED_dispatch <- rbind(ED_P, ED_P_R, ED_P_S) %>%
  group_by(time, fuel) %>%
  summarise(output = sum(output))

ED_dispatch$fuel <- factor(ED_dispatch$fuel, levels = fuel_levels)

ED_dispatch_hourly <- merge(ED_dispatch, date_tool, by.x = "time", by.y = "hour", all.x = TRUE)

ED_dispatch_daily <- merge(ED_dispatch, date_tool, by.x = "time", by.y = "hour", all.x = TRUE) %>% 
  group_by(day, fuel, quarter, weekday_number) %>%
  summarise(output= sum(output))

# PHS demand
if (!exists("ED_D_S")) ED_D_S <- multiCSVtoDF("ED_D_S.csv")   # Electricity demand of storages units

ED_D_S <- ED_D_S %>% 
  group_by(time, fuel) %>%
  summarise(output = sum(output))

ED_D_S_hourly <- merge(ED_D_S, date_tool, by.x = "time", by.y = "hour", all.x = TRUE)

ED_D_S_daily <- merge(ED_D_S, date_tool, by.x = "time", by.y = "hour", all.x = TRUE) %>% 
  group_by(day, fuel, quarter, weekday_number) %>%
  summarise(output = sum(output))

#############################
### MARKET CLEARING PRICE ### 
#############################

### Market clearing price DATA
if (!exists("ED_price")) ED_price <- multiCSVtoDF("ED_price.csv")   
ED_price_hourly <- merge(ED_price, date_tool, by.x = "time", by.y = "hour", all.x = TRUE)

ED_price_daily <- ED_price_hourly %>%
  group_by(day, quarter) %>%
  summarise(price_mean = mean(price),
            price_max = max(price),
            price_min = min(price))

#############################
### CONGESTION MANAGEMENT ###
#############################

if (!exists("CM_P_up")) CM_P_up <- multiCSVtoDF("CM_P_up.csv")        # Upward and downward adjustment of dispatchable power plants
if (!exists("CM_P_dn")) CM_P_dn <- multiCSVtoDF("CM_P_dn.csv")   
if (!exists("CM_P_R_up")) CM_P_R_up <- multiCSVtoDF("CM_P_R_up.csv")  # Upward and downward adjustment of renewable energy units
if (!exists("CM_P_R_dn")) CM_P_R_dn <- multiCSVtoDF("CM_P_R_dn.csv")
if (!exists("CM_P_S_up")) CM_P_S_up <- multiCSVtoDF("CM_P_S_up.csv")  # Upward adjustment of generation by storage units

### Lost load
if (!exists("CM_P_load_lost")) CM_P_load_lost <- multiCSVtoDF("CM_P_load_lost.csv") 
CM_P_load_lost$fuel = "Lost load"
CM_P_load_lost <- CM_P_load_lost %>% 
  group_by(time, fuel) %>%
  summarise(output = sum(output))

### Lost generation
if (!exists("CM_P_gen_lost")) CM_P_gen_lost <- multiCSVtoDF("CM_P_gen_lost.csv") 
CM_P_gen_lost$fuel = "Lost gen"
CM_P_gen_lost <- CM_P_gen_lost %>% 
  group_by(time, fuel) %>%
  summarise(output = sum(output))

CM_P_gen_lost$output <- -1*CM_P_gen_lost$output


# Total redispatch
CM_redispatch_up <- rbind(CM_P_up, CM_P_R_up, CM_P_S_up) %>% 
  group_by(time, fuel) %>%
  summarise(output = sum(output))

CM_redispatch_up <- rbind(CM_redispatch_up, CM_P_load_lost)
CM_redispatch_up$type = "up"

CM_redispatch_dn <- rbind(CM_P_dn, CM_P_R_dn) %>% 
  group_by(time, fuel) %>%
  summarise(output = sum(output))

CM_redispatch_dn <- rbind(CM_redispatch_dn, CM_P_gen_lost)
CM_redispatch_dn$type = "dn"

CM_redispatch <- rbind(CM_redispatch_up, CM_redispatch_dn) 

CM_redispatch$fuel <- factor(CM_redispatch$fuel, levels = fuel_levels_lost)

CM_redispatch_daily <- merge(CM_redispatch, date_tool, by.x = "time", by.y = "hour", all.x = TRUE) %>% 
  group_by(day, fuel, type) %>%
  summarise(output= sum(output))

# Total redispatch summed up over time
CM_redispatch_total <- CM_redispatch %>% group_by(fuel) %>%
  summarise(output = sum(output))

###################################
### CONGESTION MANAGEMENT + PTG ###
###################################

if (!exists("CM_PtG_P_up")) CM_PtG_P_up <- multiCSVtoDF("CM_PtG_P_up.csv")        # Upward and downward adjustment of dispatchable power plants
if (!exists("CM_PtG_P_dn")) CM_PtG_P_dn <- multiCSVtoDF("CM_PtG_P_dn.csv") 
if (!exists("CM_PtG_P_R_up")) CM_PtG_P_R_up <- multiCSVtoDF("CM_PtG_P_R_up.csv")  # Upward and downward adjustment of renewable energy units
if (!exists("CM_PtG_P_R_dn")) CM_PtG_P_R_dn <- multiCSVtoDF("CM_PtG_P_R_dn.csv")
if (!exists("CM_PtG_P_S_up")) CM_PtG_P_S_up <- multiCSVtoDF("CM_PtG_P_S_up.csv")  # Upward and downward adjustment of generation by storage units

if (!exists("CM_PtG_P_syn")) CM_PtG_P_syn <- multiCSVtoDF("CM_PtG_P_syn.csv")     # PtG demand and SNG usage
if (!exists("CM_PtG_D_PtG")) CM_PtG_D_PtG <- multiCSVtoDF("CM_PtG_D_PtG.csv")
if (!exists("CM_PtG_L_syn")) CM_PtG_L_syn <- multiCSVtoDF("CM_PtG_L_syn.csv")     # PtG storage level

### Lost load
if (!exists("CM_PtG_P_load_lost")) CM_PtG_P_load_lost <- multiCSVtoDF("CM_PtG_P_load_lost.csv") 
CM_PtG_P_load_lost$fuel = "Lost load"
CM_PtG_P_load_lost <- CM_PtG_P_load_lost %>% 
  group_by(time, fuel) %>%
  summarise(output = sum(output))

### Lost generation
if (!exists("CM_PtG_P_gen_lost")) CM_PtG_P_gen_lost <- multiCSVtoDF("CM_PtG_P_gen_lost.csv") 
CM_PtG_P_gen_lost$fuel = "Lost gen"
CM_PtG_P_gen_lost <- CM_PtG_P_gen_lost %>% 
  group_by(time, fuel) %>%
  summarise(output = sum(output))

CM_PtG_P_gen_lost$output <- -1*CM_PtG_P_gen_lost$output

CM_PtG_D_PtG$output <- -1*CM_PtG_D_PtG$output

# Grouped PtG demand
CM_PtG_D_PtG_grouped <- CM_PtG_D_PtG %>%
  group_by(time, fuel) %>%
  summarise(output = -1*sum(output))

# Total redispatch
CM_PtG_redispatch_up <- rbind(CM_PtG_P_up, CM_PtG_P_R_up, CM_PtG_P_S_up, CM_PtG_P_syn) %>% 
  group_by(time, fuel) %>%
  summarise(output = sum(output))

CM_PtG_redispatch_up <- rbind(CM_PtG_redispatch_up, CM_PtG_P_load_lost)
CM_PtG_redispatch_up$type = "up"

CM_PtG_redispatch_dn <- rbind(CM_PtG_P_dn, CM_PtG_P_R_dn, CM_PtG_D_PtG) %>% 
  group_by(time, fuel) %>%
  summarise(output = sum(output))

CM_PtG_redispatch_dn <- rbind(CM_PtG_redispatch_dn, CM_PtG_P_gen_lost)
CM_PtG_redispatch_dn$type = "dn"

CM_PtG_redispatch <- rbind(CM_PtG_redispatch_up, CM_PtG_redispatch_dn)
CM_PtG_redispatch$fuel <- factor(CM_PtG_redispatch$fuel, levels = fuel_levels_ext_lost)

CM_PtG_redispatch_hourly <- merge(CM_PtG_redispatch, date_tool, by.x = "time", by.y = "hour", all.x = TRUE)


CM_PtG_redispatch_daily <- merge(CM_PtG_redispatch, date_tool, by.x = "time", by.y = "hour", all.x = TRUE) %>% 
  group_by(day, fuel, type) %>%
  summarise(output= sum(output))

# Total redispatch summed up over time
CM_PtG_redispatch_total <- CM_PtG_redispatch %>% group_by(fuel) %>%
  summarise(output = sum(output))

#################
### PIE CHART ###
#################

# ED total
ED_dispatch_total <- ED_dispatch %>% group_by(fuel) %>%
  summarise(output = sum(output))
ED_dispatch_total$source <- "ED"
ED_dispatch_total$output_percent <- ED_dispatch_total$output/sum(ED_dispatch_total$output) * 100

# ED + CM
ED_dispatch_after_CM_total <- merge(ED_dispatch_total, CM_redispatch_total, by.x = "fuel", by.y = "fuel", all.x = TRUE)
ED_dispatch_after_CM_total[is.na(ED_dispatch_after_CM_total)] <- 0
ED_dispatch_after_CM_total$source <- "ED + CM"
ED_dispatch_after_CM_total$output <- ED_dispatch_after_CM_total$output.x + ED_dispatch_after_CM_total$output.y
ED_dispatch_after_CM_total <- ED_dispatch_after_CM_total[, c("fuel", "output", "source")]
ED_dispatch_after_CM_total$output_percent <- ED_dispatch_after_CM_total$output/sum(ED_dispatch_after_CM_total$output) * 100

# ED + CM and Pumped Hydro
ED_dispatch_after_CM_total[ED_dispatch_after_CM_total$fuel == "Hydro", ]$output = ED_dispatch_after_CM_total[ED_dispatch_after_CM_total$fuel == "Hydro", ]$output

# Bind all dataframes
data_pie <- rbind(ED_dispatch_total, ED_dispatch_after_CM_total, data_nettostrom_2015)
data_pie$label <- round(data_pie$output_percent, 1)
data_pie$label[data_pie$label < 1] <- ""
data_pie$label[data_pie$label != ""] <- paste(data_pie$label[data_pie$label != ""], "%")
data_pie$source <- factor(data_pie$source, levels = c("BNetzA", "ED", "ED + CM"))
data_pie$source_label[data_pie$fuel == "WindOnshore"] <- as.character(data_pie$source[data_pie$fuel == "WindOnshore"])

###########################
### CM + PTG MECHANISMS ###
###########################

# Must-run power
if (!exists("data_pmin")) data_pmin <- multiCSVtoDF("pmin.csv") 
data_pmin_grouped <- data_pmin %>%
  group_by(time, fuel) %>%
  summarise(output = sum(output))

# Max RES
if (!exists("data_maxres")) data_maxres <- multiCSVtoDF("MaxRES.csv") 
data_maxres_grouped <- data_maxres %>%
  group_by(time, fuel) %>%
  summarise(output = sum(output))

# Data for CM + PtG mechanism
data_pmin_maxres <- rbind(data_pmin_grouped, data_maxres_grouped)
data_pmin_maxres$fuel <- factor(data_pmin_maxres$fuel, levels = fuel_levels)


#################
### LINE FLOW ###
#################

if (!exists("CM_P_flow")) CM_P_flow <- multiCSVtoDF("CM_P_flow.csv")              # CM
if (!exists("CM_PtG_P_flow")) CM_PtG_P_flow <- multiCSVtoDF("CM_PtG_P_flow.csv")  # CM + PtG

##############
### LOADED ###
##############

load_results <- TRUE
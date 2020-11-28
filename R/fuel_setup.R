# Fuel factor levels
fuel_levels <- c("WindOnshore", "WindOffshore", "SolarPV", "NaturalGas", "HardCoal", "Lignite", "Nuclear",
                 "Oil")



fuel_levels_ext <- c("SNG", "PtG", "WindOnshore", "WindOffshore", "SolarPV", "NaturalGas", "HardCoal", "Lignite", "Nuclear",
                     "Oil")

# Fuel colors
fcolors_data <- read.csv(file = "R/fuelcolors.csv") 
fcolors <- fcolors_data$color %>% as.character()
names(fcolors) <- fcolors_data$fuel

# Fuel names
flabels <- fcolors_data$label %>% as.character()
names(flabels) <- fcolors_data$fuel
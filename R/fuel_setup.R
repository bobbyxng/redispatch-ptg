# Fuel factor levels
fuel_levels <- c("WindOnshore", "WindOffshore", "SolarPV", "Biomass", "RoR", "Hydro", "Geothermal", "NaturalGas", "HardCoal", "Lignite", "Nuclear",
                 "Oil(light)", "Oil(heavy)", "Waste", "OtherFuels")



fuel_levels_ext <- c("SNG", "PtG", "WindOnshore", "WindOffshore", "SolarPV", "Biomass", "RoR", "Hydro", "Geothermal", "NaturalGas", 
                     "HardCoal", "Lignite", "Nuclear", "Oil(light)", "Oil(heavy)", "Waste", "OtherFuels")

fuel_levels_lost <- c("WindOnshore", "WindOffshore", "SolarPV", "Biomass", "RoR", "Hydro", "Geothermal", "NaturalGas", "HardCoal", "Lignite",  "Nuclear",
                      "Oil(light)", "Oil(heavy)", "Waste", "OtherFuels", "Lost load", "Lost gen")

fuel_levels_ext_lost <- c("SNG", "PtG", "WindOnshore", "WindOffshore", "SolarPV", "Biomass", "RoR", "Hydro", "Geothermal", "NaturalGas", 
                          "HardCoal", "Lignite", "Nuclear", "Oil(light)", "Oil(heavy)", "Waste", "OtherFuels", "Lost load", "Lost gen")

# Fuel colors
fcolors_data <- read.csv(file = "R/fuelcolors_DE.csv") 
fcolors <- fcolors_data$color %>% as.character()
names(fcolors) <- fcolors_data$fuel

# Fuel names
flabels <- fcolors_data$label %>% as.character()
names(flabels) <- fcolors_data$fuel
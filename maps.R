############################################
### SESAM Master thesis -- Plotting tool ###
############################################

# Authors: Bobby Xiong, Johannes Predel
# (C) 2020

#######################
### Input settings ####
#######################

session <- c("2020-06-17__17-40-11__ptdf__elmod_cal_noexchange__1-8760_trm25/1-2190",
             "2020-06-17__17-40-11__ptdf__elmod_cal_noexchange__1-8760_trm25/2191-4380",
             "2020-06-17__17-40-11__ptdf__elmod_cal_noexchange__1-8760_trm25/4381-6570",
             "2020-06-17__17-40-11__ptdf__elmod_cal_noexchange__1-8760_trm25/6571-8760")

input_data <- "elmod_cal_exchange"

#############
### SETUP ###
#############

### Libraries
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(leaflet.minicharts)
library(htmltools)
library(htmlwidgets)
library(webshot2)
library(RColorBrewer)

### Directory
session_dir <- paste("output/", session, "/", sep = "")
if (!dir.exists(paste(session_dir[1], "maps", sep = ""))) dir.create(paste(session_dir[1], "maps", sep = ""))

input_data_dir <- paste("data/", input_data, "/", sep = "")

source("R/util.R", echo = FALSE)        # Functions
source("R/fuel_setup.R", echo = FALSE)  # Fuel settings
if (!exists("load_results")) source("R/load_results.R", echo = TRUE)    # Load data from results in CSV

######################
### SHAPE FILES ######
######################

shapeDE <- readOGR("shapefiles", "NUTS_RG_03M_2013_4326") %>%
  subset(CNTR_CODE == "DE") %>%
  subset(LEVL_CODE == 0)

shapeNeighbours <- readOGR("shapefiles", "NUTS_RG_03M_2013_4326") %>%
  subset(CNTR_CODE != "DE") %>%
  # subset(CNTR_CODE == "AT" |
  #   CNTR_CODE == "BE" |
  #   CNTR_CODE == "CH" |
  #   CNTR_CODE == "CZ" |
  #   CNTR_CODE == "DK" |
  #   CNTR_CODE == "FR" |
  #   CNTR_CODE == "LU" |
  #   CNTR_CODE == "NL" |
  #   CNTR_CODE == "PL"
  # ) %>%
  subset(LEVL_CODE == 0)

shapeLines <- readOGR("shapefiles", "grid_lines_DE")
shapeNodes <- readOGR("shapefiles", "grid_nodes_DE")

#####################
### COLOR PALETTE ###
#####################

palLines <- colorNumeric(
  palette = "inferno",
  domain = c(0:1),
  reverse = TRUE)

######################
### DATA WRANGLING ###
######################

# Time frame
hours = 1:8760

# Load
data_load <- read.csv(file = paste(input_data_dir, "load.csv", sep = ""), check.names = FALSE)
data_load[, 2:ncol(data_load)] <- data_load[, 2:ncol(data_load)] * data_load$systemload
data_load <- data_load[, 2:ncol(data_load)]
data_load <- cbind(time = 1:nrow(data_load), data_load)
data_load <- gather(data_load, node, load, -time)
data_load <- merge(x = data_load,
                   y = data_nodes,
                   by.x = "node",
                   by.y = "id"
)
data_load$radius <- 2e1*sqrt(data_load$load/pi)

# Cumulated load
data_load_sum <- data_load %>% 
  group_by(node, latitude, longitude) %>%
  summarise(load = sum(load))
data_load_sum$radius <- 2e1*sqrt(data_load_sum$load/pi)

# Generation
# Total dispatch 
ED_dispatch_node <- rbind(ED_P, ED_P_R, ED_P_S) %>%
  group_by(time, fuel, node) %>%
  summarise(output = sum(output))

ED_dispatch_node$fuel <- factor(ED_dispatch_node$fuel, levels = fuel_levels)

ED_dispatch_node_total <- ED_dispatch_node %>% group_by(node, fuel) %>%
  summarise(output = sum(output))

ED_dispatch_node_total <- merge(x = ED_dispatch_node_total,
                                y = data_nodes,
                                by.x = "node",
                                by.y = "id"
)

ED_dispatch_node_total$radius <- 2e1*sqrt(ED_dispatch_node_total$output/pi)

ED_dispatch_node_total_wide <- spread(ED_dispatch_node_total, fuel, output)
ED_dispatch_node_total_wide[is.na(ED_dispatch_node_total_wide)] <- 0
ED_dispatch_node_total_wide$width <- 2*sqrt(rowSums(subset(ED_dispatch_node_total_wide, select = -c(node, latitude, longitude))) *100/pi)

##########
### CM ###
##########

# Total redispatch
CM_redispatch_node <- rbind(CM_P_up, CM_P_dn, CM_P_R_up, CM_P_R_dn) 

if (exists("CM_P_S_up")) {
  CM_redispatch_node <- rbind(CM_P_up, CM_P_dn, CM_P_R_up, CM_P_R_dn, CM_P_S_up) 
}

CM_redispatch_node <- CM_redispatch_node %>%
  group_by(time, node, fuel) %>%
  summarise(output = sum(output))

CM_redispatch_node$fuel <- factor(CM_redispatch_node$fuel, levels = fuel_levels)

CM_redispatch_node_total <- CM_redispatch_node %>% group_by(node, fuel) %>%
  summarise(output = sum(output))

CM_redispatch_node_total <- merge(x = CM_redispatch_node_total,
                                  y = data_nodes,
                                  by.x = "node",
                                  by.y = "id"
)

CM_redispatch_node_total$radius <- 2e1*sqrt(abs(CM_redispatch_node_total$output)/pi)

### Line flow

CM_P_flow <- merge(CM_P_flow, select(data_lines, id, pmax_circuits),by.x = "line", by.y = "id")
CM_P_flow$pflow_abs <- abs(CM_P_flow$pflow)
CM_P_flow_grouped <- CM_P_flow %>% 
  group_by(line, from, to) %>% 
  summarise(pmax = mean(pmax), 
            pmax_circuits = mean(pmax_circuits),
            pflow_avg = mean(pflow_abs),
            pflow_avg_percent = mean(pflow_abs)/pmax_circuits,
            pflow_max = max(pflow_abs),
            pflow_max_percent = max(pflow_abs)/pmax_circuits)

CM_P_flow_grouped$pflow_avg_col <- palLines(CM_P_flow_grouped$pflow_avg_percent)
CM_P_flow_grouped$pflow_max_col <- palLines(CM_P_flow_grouped$pflow_max_percent)


################
### CM + PtG ###
################

# Total redispatch
CM_PtG_redispatch_node <- rbind(CM_PtG_P_up, CM_PtG_P_dn, CM_PtG_P_R_up, CM_PtG_P_R_dn, CM_PtG_P_S_up) 


CM_PtG_redispatch_node <- CM_PtG_redispatch_node %>%
  group_by(time, node, fuel) %>%
  summarise(output = sum(output))

CM_PtG_redispatch_node$fuel <- factor(CM_PtG_redispatch_node$fuel, levels = fuel_levels)

CM_PtG_redispatch_node_total <- CM_PtG_redispatch_node %>% group_by(node, fuel) %>%
  summarise(output = sum(output))

CM_PtG_redispatch_node_total <- merge(x = CM_PtG_redispatch_node_total,
                                      y = data_nodes,
                                      by.x = "node",
                                      by.y = "id")

CM_PtG_redispatch_node_total$radius <- 2e1*sqrt(abs(CM_PtG_redispatch_node_total$output)/pi)

### Line flow
CM_PtG_P_flow <- merge(CM_PtG_P_flow, select(data_lines, id, pmax_circuits),by.x = "line", by.y = "id")
CM_PtG_P_flow$pflow_abs <- abs(CM_PtG_P_flow$pflow)
CM_PtG_P_flow_grouped <- CM_PtG_P_flow %>% 
  group_by(line, from, to) %>% 
  summarise(pmax = mean(pmax), 
            pmax_circuits = mean(pmax_circuits),
            pflow_avg = mean(pflow_abs),
            pflow_avg_percent = mean(pflow_abs)/pmax_circuits,
            pflow_max = max(pflow_abs),
            pflow_max_percent = max(pflow_abs)/pmax_circuits)

CM_PtG_P_flow_grouped$pflow_avg_col <- palLines(CM_PtG_P_flow_grouped$pflow_avg_percent)
CM_PtG_P_flow_grouped$pflow_max_col <- palLines(CM_PtG_P_flow_grouped$pflow_max_percent)

# PtG demand and SNG usage
CM_PtG_P_syn_total <- CM_PtG_P_syn %>% 
  group_by(node, fuel) %>%
  summarise(output = sum(output)) %>% filter(output > 0)

CM_PtG_P_syn_total <- merge(x = CM_PtG_P_syn_total,
                            y = data_nodes,
                            by.x = "node",
                            by.y = "id")

CM_PtG_P_syn_total$radius <- 2e1*sqrt(abs(CM_PtG_P_syn_total$output)/pi)

CM_PtG_D_PtG_total <- CM_PtG_D_PtG %>% 
  group_by(node, fuel) %>%
  summarise(output = sum(output)) %>% filter(output < 0)

CM_PtG_D_PtG_total <- merge(x = CM_PtG_D_PtG_total,
                            y = data_nodes,
                            by.x = "node",
                            by.y = "id")

CM_PtG_D_PtG_total$radius <- 2e1*sqrt(abs(CM_PtG_D_PtG_total$output)/pi)

######################
### HTML #############
######################

contentLines <- paste(sep = "",
                      "<b><small>Line ", shapeLines@data$gid, ": </small></b><br/>", 
                      shapeLines@data$From_node, " — ",shapeLines@data$To_node, "<hr/>",
                      "<small>",
                      "<b>Voltage: </b>", shapeLines@data$voltage, " kV<br/>",
                      "<b>Capacity: </b>", shapeLines@data$Capacity_c, " MW<br/>",
                      "<b>Circuits: </b>", shapeLines@data$Circuits, "<br/>",
                      "<b>Status: </b><i>", shapeLines@data$status, "</i><br/>",
                      "</small>"
)

contentNodes <- paste(sep = "",
                      "<b><small>Node ", shapeNodes@data$gid, ":</small></b><br/>", 
                      shapeNodes@data$Name
)

######################
### Functions ########
######################

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

saveWidgetFix <- function(widget, file, ...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd <- getwd()
  on.exit(setwd(wd))
  outDir <- dirname(file)
  file <- basename(file)
  setwd(outDir);
  saveWidget(widget, file = file,...)
}


addFuelGeneration <- function(map, fuel) {
  data = ED_dispatch_node_total[ED_dispatch_node_total$fuel == fuel, ]
  
  addCircles(map = map,
             data = data,
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius,
             fillColor = as.list(fcolors)[[fuel]],
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = as.list(flabels)[[fuel]],
             options = pathOptions(pane = "paneFuelGeneration")
  )
}

######################
### CSS and JS #######
######################

cssLeaflet <- htmlDependency(name = "cssLeaflet",
                             version = "1.0",
                             src = c(file = paste(sep = "", getwd(),"/script/")),
                             stylesheet = "leaflet.css"
)

cssFA <- htmlDependency(name = "font-awesome",
                        version = "4.3.0",
                        src = c(href = "http://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css"),
                        stylesheet = "font-awesome.min.css"
)

jsZoomHome <- htmlDependency(name = "leaflet.zoomhome",
                             version = "0.2.0",
                             src = c(file = paste(sep = "", getwd(),"/script/")),
                             script = "leaflet.zoomhome.min.js"
)

#########################
### LEAFLET CM ##########
#########################

m <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
  
  # setView(lng = sum(bbox(shapeLines)[c(1, 3)])/2,
  #         lat = sum(bbox(shapeLines)[c(2, 4)])/2,
  #         zoom = 6) %>%
  
  fitBounds(lng1 = bbox(shapeLines)[1], 
            lat1 = bbox(shapeLines)[2], 
            lng2 = bbox(shapeLines)[3], 
            lat2 = bbox(shapeLines)[4]) %>%
  
  # Map panes
  addMapPane("paneDE", zIndex = 401) %>%
  addMapPane("paneLoad", zIndex = 411) %>%
  addMapPane("paneFuelGeneration", zIndex = 412) %>%
  # addMapPane("paneLines", zIndex = 432) %>%
  addMapPane("paneLines", zIndex = 405) %>%
  addMapPane("paneCM", zIndex = 433) %>%
  # addMapPane("paneMarkers", zIndex = 601) %>%
  addMapPane("paneMarkers", zIndex = 404) %>%
  
  # Overlay groups
  addCircleMarkers(data = shapeNodes,
                   radius = 2, 
                   stroke = TRUE,
                   color = "#EEEEEE",
                   weight = 1,
                   fillColor = "#EEEEEE",
                   fillOpacity = 1,
                   group = "Nodes",
                   label = ~paste(sep = "", shapeNodes@data$gid, ": ",shapeNodes@data$Name), 
                   popup = contentNodes, 
                   options = pathOptions(pane = "paneMarkers")
  ) %>%
  
  addCircles(data = data_load_sum,
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius,
             stroke = FALSE,
             label = ~paste(round(load/1e6,1), "TWh"),
             group = "Load",
             options = pathOptions(pane = "paneLoad")
  ) %>%
  
  # addMinicharts(chartdata = subset(ED_dispatch_node_total_wide, select = -c(node, latitude, longitude, width)),
  #               lng = ED_dispatch_node_total_wide$longitude,
  #               lat = ED_dispatch_node_total_wide$latitude,
  #               type = "pie",
  #               colorPalette = as.vector(fcolors),
  #               width = ED_dispatch_node_total_wide$width,
  #               showLabels = TRUE,
  # ) %>%
  
  addPolygons(data = shapeNeighbours,
              color = "black",
              fillColor = "gray",
              dashArray = "...",
              weight = 1,
              group = "NUTS"
  ) %>%
  
  addPolygons(data = shapeDE,
              color = "black",
              fillColor = "white",
              weight = 1,
              group = "NUTS",
              options = pathOptions(pane = "paneDE")
  ) %>%
  
  addPolylines(data = shapeLines,
               weight = 2,
               opacity = 1,
               col = "#EEEEEE",
               group = "Lines",
               label = ~paste(sep = "", shapeLines@data$From_node, " — ",shapeLines@data$To_node),
               popup = contentLines,
               options = pathOptions(pane = "paneLines"),
               highlightOptions = highlightOptions(color = "green",
                                                   weight = 4, 
                                                   bringToFront = TRUE, 
                                                   opacity = 1)
  ) %>%
  
  addPolylines(data = shapeLines,
               weight = 2,
               opacity = 1,
               col = CM_P_flow_grouped$pflow_avg_col,
               group = "Line utilisation (avg.)",
               label = ~paste(sep = "", shapeLines@data$From_node, " — ",shapeLines@data$To_node, ": ", 
                              round(100*CM_P_flow_grouped$pflow_avg_percent, 2)," %"),
               popup = contentLines,
               options = pathOptions(pane = "paneLines"),
               highlightOptions = highlightOptions(weight = 4, 
                                                   bringToFront = TRUE, 
                                                   opacity = 1)
  ) %>%
  
  addPolylines(data = shapeLines,
               weight = 2,
               opacity = 1,
               col = CM_P_flow_grouped$pflow_max_col,
               group = "Line utilisation (max.)",
               label = ~paste(sep = "", shapeLines@data$From_node, " — ",shapeLines@data$To_node, ": ", 
                              round(100*CM_P_flow_grouped$pflow_max_percent, 2)," %"),
               popup = contentLines,
               options = pathOptions(pane = "paneLines"),
               highlightOptions = highlightOptions(weight = 4, 
                                                   bringToFront = TRUE, 
                                                   opacity = 1)
  ) %>%
  
  # Upwards change
  addCircles(data = filter(CM_redispatch_node_total, output > 0),
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius,
             fillColor = "green",
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = "Upwards change",
             options = pathOptions(pane = "paneCM")) %>%
  
  # Downwards change
  addCircles(data = filter(CM_redispatch_node_total, output < 0),
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius,
             fillColor = "red",
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = "Downwards change",
             options = pathOptions(pane = "paneCM")) %>%
  
  # Customise CSS and register
  registerPlugin(cssLeaflet)


# Interactive html gets more features not required in PDF output
m_html <- m %>%
  
  # Base groups
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(noWrap = TRUE),
                   group = "Carto (Positron)"
  ) %>%
  
  addProviderTiles(providers$CartoDB.DarkMatter,
                   options = providerTileOptions(noWrap = TRUE),
                   group = "Carto (Dark Matter)"
  ) %>%
  
  # Layers control
  addLayersControl(baseGroups = c("NUTS", "Carto (Positron)", "Carto (Dark Matter)"),
                   overlayGroups = c("Nodes", "Lines", "Line utilisation (avg.)", "Line utilisation (max.)", "Load",
                                     "Upwards change", "Downwards change",       
                                     as.vector(flabels[levels(ED_dispatch_node_total$fuel)])),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright"
  ) %>%
  
  hideGroup(c("Line utilisation (avg.)","Line utilisation (max.)", "Load", "Upwards change", "Downwards change",
              as.vector(flabels[levels(ED_dispatch_node_total$fuel)]))) %>%
  
  registerPlugin(cssFA) %>%
  registerPlugin(jsZoomHome) %>%
  
  # Full screen toggle
  addFullscreenControl(position = "topright") %>%
  
  # Additional leaflet functionality]
  # leafem::addMouseCoordinates() %>%
  
  onRender("function(el,x) {
    var zoomHome = L.Control.zoomHome({position: 'topright'});
    zoomHome.addTo(this);}") %>%
  
  # Hash value in URL
  addHash() %>%
  
  # Search function for nodes
  addSearchFeatures(targetGroups = "Nodes",
                    options = searchFeaturesOptions(
                      zoom=7, openPopup = TRUE, firstTipSubmit = TRUE,
                      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE))


# Add all fuels
for (fuel in levels(ED_dispatch_node_total$fuel)){
  m_html <- addFuelGeneration(m_html, fuel)
}


######################
### Export HTML ######
######################

saveWidgetFix(m_html, file = paste(session_dir[1], "maps/", "map.html", sep = ""), selfcontained = TRUE)
saveWidgetFix(m, file = paste(session_dir[1], "maps/", "map_pdf.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_pdf.html"), 
        file = paste(session_dir[1], "maps/", "map.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))


###############################
### LEAFLET CM + PtG ##########
###############################

m_ptg <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
  
  # setView(lng = sum(bbox(shapeLines)[c(1, 3)])/2,
  #         lat = sum(bbox(shapeLines)[c(2, 4)])/2,
  #         zoom = 6) %>%
  
  fitBounds(lng1 = bbox(shapeLines)[1], 
            lat1 = bbox(shapeLines)[2], 
            lng2 = bbox(shapeLines)[3], 
            lat2 = bbox(shapeLines)[4]) %>%
  
  # Map panes
  addMapPane("paneDE", zIndex = 401) %>%
  addMapPane("paneLoad", zIndex = 411) %>%
  addMapPane("paneFuelGeneration", zIndex = 412) %>%
  # addMapPane("paneLines", zIndex = 432) %>%
  addMapPane("paneLines", zIndex = 405) %>%
  addMapPane("paneCM", zIndex = 433) %>%
  # addMapPane("paneMarkers", zIndex = 601) %>%
  addMapPane("paneMarkers", zIndex = 404) %>%
  
  # Overlay groups
  addCircleMarkers(data = shapeNodes,
                   radius = 2, 
                   stroke = TRUE,
                   color = "#EEEEEE",
                   weight = 1,
                   fillColor = "#EEEEEE",
                   fillOpacity = 1,
                   group = "Nodes",
                   label = ~paste(sep = "", shapeNodes@data$gid, ": ",shapeNodes@data$Name), 
                   popup = contentNodes, 
                   options = pathOptions(pane = "paneMarkers")
  ) %>%
  
  addCircles(data = data_load_sum,
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius,
             stroke = FALSE,
             label = ~paste(round(load/1e6,1), "TWh"),
             group = "Load",
             options = pathOptions(pane = "paneLoad")
  ) %>%
  
  # addMinicharts(chartdata = subset(ED_dispatch_node_total_wide, select = -c(node, latitude, longitude, width)),
  #               lng = ED_dispatch_node_total_wide$longitude,
  #               lat = ED_dispatch_node_total_wide$latitude,
  #               type = "pie",
  #               colorPalette = as.vector(fcolors),
  #               width = ED_dispatch_node_total_wide$width,
  #               showLabels = TRUE,
  # ) %>%
  
  addPolygons(data = shapeNeighbours,
              color = "black",
              fillColor = "gray",
              dashArray = "...",
              weight = 1,
              group = "NUTS"
  ) %>%
  
  addPolygons(data = shapeDE,
              color = "black",
              fillColor = "white",
              weight = 1,
              group = "NUTS",
              options = pathOptions(pane = "paneDE")
  ) %>%
  
  addPolylines(data = shapeLines,
               weight = 2,
               opacity = 1,
               col = "#EEEEEE",
               group = "Lines",
               label = ~paste(sep = "", shapeLines@data$From_node, " — ",shapeLines@data$To_node),
               popup = contentLines,
               options = pathOptions(pane = "paneLines"),
               highlightOptions = highlightOptions(color = "green",
                                                   weight = 4, 
                                                   bringToFront = TRUE, 
                                                   opacity = 1)
  ) %>%
  
  addPolylines(data = shapeLines,
               weight = 2,
               opacity = 1,
               col = CM_PtG_P_flow_grouped$pflow_avg_col,
               group = "Line utilisation (avg.)",
               label = ~paste(sep = "", shapeLines@data$From_node, " — ",shapeLines@data$To_node, ": ", 
                              round(100*CM_PtG_P_flow_grouped$pflow_avg_percent, 2)," %"),
               popup = contentLines,
               options = pathOptions(pane = "paneLines"),
               highlightOptions = highlightOptions(weight = 4, 
                                                   bringToFront = TRUE, 
                                                   opacity = 1)
  ) %>%
  
  addPolylines(data = shapeLines,
               weight = 2,
               opacity = 1,
               col = CM_PtG_P_flow_grouped$pflow_max_col,
               group = "Line utilisation (max.)",
               label = ~paste(sep = "", shapeLines@data$From_node, " — ",shapeLines@data$To_node, ": ", 
                              round(100*CM_PtG_P_flow_grouped$pflow_max_percent, 2)," %"),
               popup = contentLines,
               options = pathOptions(pane = "paneLines"),
               highlightOptions = highlightOptions(weight = 4, 
                                                   bringToFront = TRUE, 
                                                   opacity = 1)
  ) %>%
  
  # Upwards change
  addCircles(data = filter(CM_PtG_redispatch_node_total, output > 0),
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius,
             fillColor = "green",
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = "Upwards change",
             options = pathOptions(pane = "paneCM")) %>%
  
  # Downwards change
  addCircles(data = filter(CM_PtG_redispatch_node_total, output < 0),
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius,
             fillColor = "red",
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = "Downwards change",
             options = pathOptions(pane = "paneCM")) %>%
  
  # PtG
  addCircles(data = CM_PtG_D_PtG_total,
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius*10,
             fillColor = as.list(fcolors)[["PtG"]],
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = "PtG",
             options = pathOptions(pane = "paneCM")) %>%
  
  # SNG
  addCircles(data = CM_PtG_P_syn_total,
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius*10,
             fillColor =  as.list(fcolors)[["SNG"]],
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = "SNG",
             options = pathOptions(pane = "paneCM")) %>%
  
  # Customise CSS and register
  registerPlugin(cssLeaflet)


# Interactive html gets more features not required in PDF output
m_ptg_html <- m_ptg %>%
  
  # Base groups
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(noWrap = TRUE),
                   group = "Carto (Positron)"
  ) %>%
  
  addProviderTiles(providers$CartoDB.DarkMatter,
                   options = providerTileOptions(noWrap = TRUE),
                   group = "Carto (Dark Matter)"
  ) %>%
  
  # Layers control
  addLayersControl(baseGroups = c("NUTS", "Carto (Positron)", "Carto (Dark Matter)"),
                   overlayGroups = c("Nodes", "Lines", "Line utilisation (avg.)", "Line utilisation (max.)", "Load",
                                     "Upwards change", "Downwards change",  
                                     "PtG", "SNG",
                                     as.vector(flabels[levels(ED_dispatch_node_total$fuel)])),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright"
  ) %>%
  
  hideGroup(c("Line utilisation (avg.)","Line utilisation (max.)", "Load", "Upwards change", "Downwards change", 
              "PtG", "SNG",
              as.vector(flabels[levels(ED_dispatch_node_total$fuel)]))) %>%
  
  registerPlugin(cssFA) %>%
  registerPlugin(jsZoomHome) %>%
  
  # Full screen toggle
  addFullscreenControl(position = "topright") %>%
  
  # Additional leaflet functionality]
  # leafem::addMouseCoordinates() %>%
  
  onRender("function(el,x) {
    var zoomHome = L.Control.zoomHome({position: 'topright'});
    zoomHome.addTo(this);}") %>%
  
  # Hash value in URL
  addHash() %>%
  
  # Search function for nodes
  addSearchFeatures(targetGroups = "Nodes",
                    options = searchFeaturesOptions(
                      zoom=7, openPopup = TRUE, firstTipSubmit = TRUE,
                      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE))


# Add all fuels
for (fuel in levels(ED_dispatch_node_total$fuel)){
  m_ptg_html <- addFuelGeneration(m_ptg_html, fuel)
}


######################
### Export HTML ######
######################

saveWidgetFix(m_ptg_html, file = paste(session_dir[1], "maps/", "map_ptg.html", sep = ""), selfcontained = TRUE)
saveWidgetFix(m_ptg, file = paste(session_dir[1], "maps/", "map_ptg_pdf.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_ptg_pdf.html"), 
        file = paste(session_dir[1], "maps/", "map_ptg.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))

####################
### For PAPER ######

m_paper <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
  
  # setView(lng = sum(bbox(shapeLines)[c(1, 3)])/2,
  #         lat = sum(bbox(shapeLines)[c(2, 4)])/2,
  #         zoom = 6) %>%
  
  fitBounds(lng1 = bbox(shapeLines)[1], 
            lat1 = bbox(shapeLines)[2], 
            lng2 = bbox(shapeLines)[3], 
            lat2 = bbox(shapeLines)[4]) %>%
  
  # Map panes
  addMapPane("paneDE", zIndex = 401) %>%
  addMapPane("paneLoad", zIndex = 411) %>%
  addMapPane("paneFuelGeneration", zIndex = 412) %>%
  # addMapPane("paneLines", zIndex = 432) %>%
  addMapPane("paneLines", zIndex = 405) %>%
  addMapPane("paneCM", zIndex = 433) %>%
  # addMapPane("paneMarkers", zIndex = 601) %>%
  addMapPane("paneMarkers", zIndex = 404) %>%
  
  addPolygons(data = shapeNeighbours,
              color = "black",
              fillColor = "gray",
              dashArray = "...",
              weight = 1,
              group = "NUTS"
  ) %>%
  
  addPolygons(data = shapeDE,
              color = "black",
              fillColor = "white",
              weight = 1,
              group = "NUTS",
              options = pathOptions(pane = "paneDE")
  ) %>%
  
  addPolylines(data = shapeLines,
               weight = 3,
               opacity = 1,
               col = "#CCCCCC",
               group = "Lines",
               label = ~paste(sep = "", shapeLines@data$From_node, " — ",shapeLines@data$To_node),
               popup = contentLines,
               options = pathOptions(pane = "paneLines"),
               highlightOptions = highlightOptions(color = "green",
                                                   weight = 4, 
                                                   bringToFront = TRUE, 
                                                   opacity = 1)
  )  %>%
  registerPlugin(cssLeaflet)


####################

### Load
m_load <- m_paper %>%
  
  addCircles(data = data_load_sum,
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius,
             stroke = FALSE,
             label = ~paste(round(load/1e6,1), "TWh"),
             group = "Load",
             options = pathOptions(pane = "paneLoad")
  )

######################
### Export HTML ######
######################

saveWidgetFix(m_load, file = paste(session_dir[1], "maps/", "map_load.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_load.html"), 
        file = paste(session_dir[1], "maps/", "map_load.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))


### Wind and Solar
m_res <- m_paper

for (fuel in c("WindOffshore", "WindOnshore", "SolarPV")){
  m_res <- addFuelGeneration(m_res, fuel)
}


######################
### Export HTML ######
######################

saveWidgetFix(m_res, file = paste(session_dir[1], "maps/", "map_res.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_res.html"), 
        file = paste(session_dir[1], "maps/", "map_res.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))

### Up and downward adjustments
m_redispatch <- m_paper %>%
  
  # Upwards change
  addCircles(data = filter(CM_redispatch_node_total, output > 0),
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius,
             fillColor = "green",
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = "Upwards change",
             options = pathOptions(pane = "paneCM")) %>%
  
  # Downwards change
  addCircles(data = filter(CM_redispatch_node_total, output < 0),
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius,
             fillColor = "red",
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = "Downwards change",
             options = pathOptions(pane = "paneCM"))
  


######################
### Export HTML ######
######################

saveWidgetFix(m_redispatch, file = paste(session_dir[1], "maps/", "map_redispatch.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_redispatch.html"), 
        file = paste(session_dir[1], "maps/", "map_redispatch.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))


######################
### Export HTML ######
######################

saveWidgetFix(m_lineflow, file = paste(session_dir[1], "maps/", "map_lineflow.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_lineflow.html"), 
        file = paste(session_dir[1], "maps/", "map_lineflow.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))

### PtG and SNG
m_ptg_sng <- m_paper %>%
  
  # PtG
  addCircles(data = CM_PtG_D_PtG_total,
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius*10,
             fillColor = as.list(fcolors)[["PtG"]],
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = "PtG",
             options = pathOptions(pane = "paneCM")) %>%
  
  # SNG
  addCircles(data = CM_PtG_P_syn_total,
             lng = ~longitude,
             lat = ~latitude,
             radius = ~radius*10,
             fillColor =  as.list(fcolors)[["NaturalGas"]],
             fillOpacity = 0.7,
             stroke = FALSE,
             label = ~lapply(paste(sep = "", node, ": <br/>",round(output/1e3,1), " GWh"), HTML),
             group = "SNG",
             options = pathOptions(pane = "paneCM"))



######################
### Export HTML ######
######################

saveWidgetFix(m_ptg_sng, file = paste(session_dir[1], "maps/", "map_ptg_sng.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_ptg_sng.html"), 
        file = paste(session_dir[1], "maps/", "map_ptg_sng.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))

### Line flow
m_lineflow <- m_paper %>%
  
  addPolylines(data = shapeLines,
               weight = 8,
               opacity = 1,
               col = CM_P_flow_grouped$pflow_avg_col,
               group = "Line utilisation (avg.)",
               label = ~paste(sep = "", shapeLines@data$From_node, " — ",shapeLines@data$To_node, ": ", 
                              round(100*CM_P_flow_grouped$pflow_avg_percent, 2)," %"),
               popup = contentLines,
               options = pathOptions(pane = "paneLines"),
               highlightOptions = highlightOptions(weight = 4, 
                                                   bringToFront = TRUE, 
                                                   opacity = 1)
               
  ) %>%
  registerPlugin(cssLeaflet)


######################
### Export HTML ######
######################

saveWidgetFix(m_lineflow, file = paste(session_dir[1], "maps/", "map_lineflow.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_lineflow.html"), 
        file = paste(session_dir[1], "maps/", "map_lineflow.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))



### Line flow before CM
if (!exists("CM_P_flow_before_CM")) CM_P_flow_before_CM <- read.csv(paste(sep = "", session_dir[1], "CM_P_flow_before_CM.csv"))

CM_P_flow_before_CM <- merge(CM_P_flow_before_CM, select(data_lines, id, pmax_circuits),by.x = "line", by.y = "id")
CM_P_flow_before_CM$pflow_abs <- abs(CM_P_flow_before_CM$pflow)
CM_P_flow_before_CM_grouped <- CM_P_flow_before_CM %>% 
  group_by(line, from, to) %>% 
  summarise(pmax = mean(pmax), 
            pmax_circuits = mean(pmax_circuits),
            pflow_avg = mean(pflow_abs),
            pflow_avg_percent = mean(pflow_abs)/pmax_circuits,
            pflow_max = max(pflow_abs),
            pflow_max_percent = max(pflow_abs)/pmax_circuits)


CM_P_flow_before_CM_grouped$pflow_avg_col <- palLines(CM_P_flow_before_CM_grouped$pflow_avg_percent)
CM_P_flow_before_CM_grouped$pflow_max_col <- palLines(CM_P_flow_before_CM_grouped$pflow_max_percent)

m_lineflow_before_CM <- m_paper %>%
  
  addPolylines(data = shapeLines,
               weight = 8,
               opacity = 1,
               col = CM_P_flow_before_CM_grouped$pflow_avg_col,
               group = "Line utilisation (avg.)",
               label = ~paste(sep = "", shapeLines@data$From_node, " — ",shapeLines@data$To_node, ": ", 
                              round(100*CM_P_flow_before_CM_grouped$pflow_avg_percent, 2)," %"),
               popup = contentLines,
               options = pathOptions(pane = "paneLines"),
               highlightOptions = highlightOptions(weight = 4, 
                                                   bringToFront = TRUE, 
                                                   opacity = 1)
               
  ) %>%
  registerPlugin(cssLeaflet)


######################
### Export HTML ######
######################

saveWidgetFix(m_lineflow_before_CM, file = paste(session_dir[1], "maps/", "map_lineflow_before_CM.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_lineflow_before_CM.html"), 
        file = paste(session_dir[1], "maps/", "map_lineflow_before_CM.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))


### Line flow difference

# CM_P_flow_difference <- CM_P_flow_before_CM_grouped
# CM_P_flow_difference$pflow_avg_percent <- CM_P_flow_before_CM_grouped$pflow_avg_percent -
#   CM_P_flow_grouped$pflow_avg_percent
# 
# CM_P_flow_difference$pflow_avg_percent[CM_P_flow_difference$pflow_avg_percent < 0] <- 0
# 
# palLines3 <- colorNumeric(
#   palette = "Reds",
#   domain = c(0:25),
#   reverse = FALSE)
# 
# CM_P_flow_difference$pflow_avg_col <- palLines3(100*CM_P_flow_difference$pflow_avg_percent)
# 
# 
# 
# 
# m_lineflow_difference<- m_paper %>%
#   
#   addPolylines(data = shapeLines,
#                weight = 8,
#                opacity = 1,
#                col = CM_P_flow_difference$pflow_avg_col,
#                group = "Line utilisation (avg.)",
#                label = ~paste(sep = "", shapeLines@data$From_node, " — ",shapeLines@data$To_node, ": ", 
#                               round(100*CM_P_flow_difference$pflow_avg_percent, 2)," %"),
#                popup = contentLines,
#                options = pathOptions(pane = "paneLines"),
#                highlightOptions = highlightOptions(weight = 4, 
#                                                    bringToFront = TRUE, 
#                                                    opacity = 1)
#                
#   ) %>%
#   registerPlugin(cssLeaflet)
# 
# 
# ######################
# ### Export HTML ######
# ######################
# 
# saveWidgetFix(m_lineflow_difference, file = paste(session_dir[1], "maps/", "map_lineflow_difference.html", sep = ""), selfcontained = TRUE)
# webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_lineflow_difference.html"), 
#         file = paste(session_dir[1], "maps/", "map_lineflow_dfference.png", sep = ""), 
#         vwidth = 1900, 
#         vheight = 2400,
#         zoom = 1,
#         cliprect = c(0, 0, 1900, 2400))

### Thermal
m_thermal <- m_paper

for (fuel in c("Nuclear", "Lignite", "HardCoal", "NaturalGas")){
  m_thermal <- addFuelGeneration(m_thermal, fuel)
}


######################
### Export HTML ######
######################

saveWidgetFix(m_thermal, file = paste(session_dir[1], "maps/", "map_thermal.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_thermal.html"), 
        file = paste(session_dir[1], "maps/", "map_thermal.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))

### Flexible RES
m_res_flex <- m_paper

for (fuel in c("Biomass", "RoR", "Hydro")){
  m_res_flex <- addFuelGeneration(m_res_flex, fuel)
}


######################
### Export HTML ######
######################

saveWidgetFix(m_res_flex, file = paste(session_dir[1], "maps/", "map_res_flex.html", sep = ""), selfcontained = TRUE)
webshot(paste(sep = "", getwd(), "/", session_dir[1], "maps/map_res_flex.html"), 
        file = paste(session_dir[1], "maps/", "map_res_flex.png", sep = ""), 
        vwidth = 1900, 
        vheight = 2400,
        zoom = 1,
        cliprect = c(0, 0, 1900, 2400))

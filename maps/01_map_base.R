##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2026-01-26
# Script Name: ~/DUC2_viewer_acoustic_telemetry/leaflet_env_data.R
# Script Description: make a leaflet map with several environmental layers as overlaygroups 

##################################################################################
##################################################################################


# install and load libraries ----------------------------------------------
# install.packages("leaflet.extras")
# install.packages("leaflet.minicharts")
# install.packages("leafem")
library(dplyr)
library(leaflet)
library(leaflet.extras)
# library(leaflet.extras2)
library(leafem)

# colors
blue_light <- "#cde3f6" 
blue_medium <- "#477292"
blue_dark <- "#11395a"


# make base leaflet map --------------------------------------------------------

north.arrow.icon <-
  "<img src='https://www.clipartbest.com/cliparts/yTo/Lgr/yToLgryGc.png' style='width:35px;height:45px;'>"


map_base <-
  leaflet() %>%
  setView(3, 51.5, zoom = 8) %>%
  addMapPane("basePane", zIndex = 100) %>%
  addTiles(group = "Open Street Map", options = WMSTileOptions(pane="basePane")) %>%
  addTiles(urlTemplate = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png",
           group = "EMODnet Bathymetry", options = WMSTileOptions(pane="basePane")) %>%
  addProviderTiles("CartoDB.Positron",
                   group = "CartoDB.Positron", options = WMSTileOptions(pane="basePane")) %>%
  leafem::addMouseCoordinates() %>%
  leaflet.extras::addFullscreenControl() %>%
  leaflet::addScaleBar(position = "bottomleft",
                       options = scaleBarOptions(
                         maxWidth = 150,
                         imperial = FALSE)) %>%  
  # north arrow
  leaflet::addControl( html = north.arrow.icon,
                       position = "topleft",
                       className = "fieldset {border: 0;}") #%>%
  
  # minimap
  # addMiniMap(position = "topright",
  #            width = 175,
  #            height = 150,
  #            zoomLevelOffset = -4,
  #            zoomLevelFixed = T,
  #            centerFixed = F,
  #            aimingRectOptions = list(color = blue_medium, weight = 1, clickable = FALSE),
  #            shadowRectOptions = list(color = blue_dark, weight = 1, clickable = FALSE, opacity = 0, fillOpacity = 0)
  #            # ,tiles = providers$Esri.WorldStreetMap
  #            , tiles = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png") #%>%
  
  # print button # loading a long time for now...investigate
  # leaflet.extras2::addEasyprint(
  #   options = leaflet.extras2::easyprintOptions(
  #     title = "Export PNG",
  #     spinnerBgColor = blue_light,
  #     filename = "DTO-Bioflow_DUC2_map",
  #     position = "topleft"))

# map_base

## save base map

saveRDS(map_base, "./maps/01_map_base.rds")

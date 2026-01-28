
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
# library(leaflet.extras) 
library(leafem)
library(htmlwidgets)
library(leaflet.minicharts) # for the map with acoustic detection data

# load the base map
map_base <- readRDS("./maps/01_map_base.rds")

# acoustic detection data map ---------------------------------------------

data <- readRDS("./data/etn_sum_seabass.rds") %>%
  dplyr::filter(!latitude == "NaN")

map_acoustic_detections <-
  map_base %>%
  addCircleMarkers(data = data %>% dplyr::filter(date == "2018-06-29" %>% as.Date()),
                   lat = ~latitude,
                   lng = ~longitude,
                   radius = ~n_individuals,
                   fillOpacity = 0.5,
                   popup = ~paste0("n_detections: ", n_detections,
                                   ", station: ", station_name,
                                   ", n_individuals: ", n_individuals))

map_acoustic_detections

# tests -------------------------------------------------------------------

deployments <- readRDS("./data/deployments.rds")
etn_monthyear_individual_sum <- readRDS("./data/etn_sum_seabass_monthyear_indivdual.rds")

selected_date <- "2021-10-01"

deployments_minichart <- # TODO: put only deployments active during selected time period
  deployments %>%
  dplyr::group_by(station_name) %>%
  dplyr::summarise(lat = mean(deploy_latitude, na.rm = T),
                   lon = mean(deploy_longitude, na.rm = T)) %>%
  dplyr::ungroup()

etn_monthyear_individual_sum_minichart <- 
  etn_monthyear_individual_sum %>%
  dplyr::filter(monthyear %>% date() == selected_date) %>%
  tidyr::pivot_wider(names_from = tag_serial_number, values_from = n_detections, names_prefix = "id_") %>%
  dplyr::ungroup()

map_mini_test <-
  map_base %>%
  addCircleMarkers(data = deployments_minichart,
                   lat = ~lat,
                   lng = ~lon,
                   radius = 5,
                   weight = 0,
                   fillOpacity = 1,
                   fillColor =  "grey") %>%
  addMinicharts(
    lng = etn_monthyear_individual_sum_minichart$deploy_longitude, 
    lat = etn_monthyear_individual_sum_minichart$deploy_latitude,
    type = "pie",
    chartdata =etn_monthyear_individual_sum_minichart %>% select(starts_with("id_")), 
    width = 60 * sqrt(etn_monthyear_individual_sum_minichart$n_detections_monthyear_station) / sqrt(etn_monthyear_individual_sum_minichart$n_detections_monthyear),
    transitionTime = 0
  ) %>% 
  # Title control
  addControl(
    html = glue::glue("
        <div style='font-size:14px;color:black;'>
          Seabass detections from {dataset_name} on {selected_date}
        </div>
      "),
    position = "bottomleft"
  ) 

map_mini_test


[# install.packages("leaflet.extras2")
# library(leaflet.extras2)
# 
# m <- leaflet() %>%
#   addTiles() %>%
#   leaflet.extras2::addEasyprint()
# 
# m
# 
# map_base %>%
#   leaflet.extras2::addEasyprint(
#     options = leaflet.extras2::easyprintOptions(
#       title = "Export PNG",
#       spinnerBgColor = blue_light,
#       filename = "DTO-Bioflow_DUC2_map",
#       position = "topleft"
#       # other options exist; see ?easyprintOptions
#     )
#   )

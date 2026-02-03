# Load map-building scripts (unchanged, from your existing code)
source("maps/02_map_environmental_layers.R")
source("maps/03_map_acoustic_detections.R")

# Wrap them so modules never depend on global variable names directly
get_env_map <- function() {
  map_WMS_EDITO_legend
}

get_acoustic_map <- function() {
  map_acoustic_detections
}

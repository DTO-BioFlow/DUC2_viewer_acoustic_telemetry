# global.R
# This file is automatically sourced before ui.R and server.R (or app.R)

# Load libraries ----------------------------------------------------------
library(shiny)
library(DT)
library(leaflet)
library(glue)
library(httr)
library(terra)
library(htmltools)
library(leaflet.minicharts)
library(leaflet.extras)
library(leafem)
library(tidyr)
library(RColorBrewer)
library(rstac)
library(purrr)
library(arrow)
library(dplyr)
library(lubridate)

# Source all R files ------------------------------------------------------
source("helpers/source_all_files.R")
source_all(path = "R")

# Source helper scripts ---------------------------------------------------
source("./helpers/load_acoustic_telemetry_GAM_s3.R")
source("./helpers/load_STAC_metadata.R")
source("./helpers/wrangle_acoustic_telemetry_data.R")

# Load data ---------------------------------------------------------------
deployments <- readRDS("data/deployments.rds")
etn_monthyear_individual_sum <- readRDS("data/etn_sum_seabass_monthyear_individual.rds")

# Load STAC metadata ------------------------------------------------------
wms_layers <- load_STAC_metadata()
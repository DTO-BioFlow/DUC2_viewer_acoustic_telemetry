# manually refresh the etn dataset sum .rds file

library(dplyr)
library(lubridate)
library(arrow)

source("helpers/load_STAC_metadata.R")             
source("helpers/wrangle_acoustic_telemetry_data.R")

build_monthyear_rds()

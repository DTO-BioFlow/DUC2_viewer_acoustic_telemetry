##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2026-01-26
# Script Name: ~/DUC2_viewer_acoustic_telemetry/leaflet_env_data.R
# Script Description: load (meta)data from the EDITO STAC catalogue to be displayed on leaflet maps.
#                     Metada for all environmental data layers, data for acoustic telemetry data from the European Tracking Network (ETN)

##################################################################################
##################################################################################


# install and load libraries ----------------------------------------------

#install.packages("rstac")
#install.packages("mregions2")
library(rstac)
library(purrr)
library(arrow)
library(dplyr)
library(lubridate)
library(leaflet)


# overview --------------------------------------------------------------------

# etn data -> done
# marine spatial plan -> DONE
# seabed habitats -> not working yet
# shipwrecks -> not STAC but done
# OWF -> DONE
# EEZ -> not STAC but done
# MSFD and Natura2000 boundaries -> DONE
# sst -> TODO


# 0. make return df -------------------------------------------------------
# this dataframe will be loaded, it will contain all WMS links and STAC collection names of the 
# (meta)data accessed in this script.

wms_registry <- tibble(env_data_name = character(),
                             collection_name = character(),
                             wms_link = character(),
                             wms_base = character(),
                             wms_layer_name = character(),
                             legend_link = character(),
                             .added_at = as.POSIXct(character(), tz = "UTC"))

# 1. access the STAC catalogue and get data collections ----------------------------

stac_endpoint_url <- 'https://catalog.dive.edito.eu/'
api_endpoint_url <- "https://api.dive.edito.eu/data/collections/"
stac_obj <- rstac::stac(stac_endpoint_url)

stac_overview <- rstac::stac('https://catalog.dive.edito.eu/catalogs')%>% rstac::get_request()

# collections
c_obj <- rstac::collections(stac_obj) %>%
  rstac::get_request()

c_all <- c_obj$collections |> vapply(`[[`, character(1), "id") %>% as_tibble()



# 2. get data layer data and metadata -------------------------------------

## ETN dataset "PhD_Gossens" -----------------------------------------------
# this is the data that DUC4.2 is partially based upon. Here, the data will be 
# 1) queried in .parquet format, 2) summarised (to #detections and #individuals per day),
# and 3) saved in "./data/"
# TODO: save tag serial numbers of individuals to calculate monthly stats from (num individuals per month)

dataset_name <- "PhD_Goossens"
etn_collection <- "animal_tracking_datasets"

## etn data collection in EDITO STAC catalogue
etn_items <- stac_obj %>%
  rstac::stac_search(collections = etn_collection) %>%
  rstac::get_request()%>%
  rstac::items_fetch()

# Loop through the features and check if dataset_name is included in the title
for (feature in etn_items$features) {
  # Extract the title and href for the feature's asset
  title <- feature$assets$data$title
  href <- feature$assets$data$href
  
  # If the dataset_name is found in the title, extract the href
  if (grepl(dataset_name, title, fixed = TRUE)) {
    etn_dataset_href <- href
    break
  }
}

# Print the matching href
#print(etn_dataset_href)

# etn_dataset <- arrow::read_parquet(etn_dataset_href, format = "parquet")
## use etn data subset as long as .parquet is not working

etn_dataset <- readRDS("./data/detections.rds")

# start <- "2021-01-01" %>% as.POSIXct()
# end <- "2022-12-31" %>% as.POSIXct()

etn_daily_sum <- 
  etn_dataset %>% 
  #dplyr::filter(datetime %>% between(start, end)) %>%
  dplyr::mutate(date = date_time %>% as.Date()) %>%
  # dplyr::mutate(date = datetime %>% as.Date()) %>% # for STAC dataset
  dplyr::group_by(date, station_name) %>%
  dplyr::summarise(
     # latitude = mean(latitude, na.rm = T),
     # longitude = mean(longitude, na.rm = T),
     deploy_latitude = mean(deploy_latitude, na.rm = T),
     deploy_longitude = mean(deploy_longitude, na.rm = T),
     n_detections = n(),
     n_individuals = tag_serial_number %>% unique() %>% length()
     # , tag_serial_nums = paste0(tag_serial_number %>% unique(), collapse = ", ") 
                     )

# save as .RDS
etn_daily_sum_path <- "./data/etn_sum_seabass.rds"
saveRDS(etn_daily_sum, etn_daily_sum_path)

etn_monthyear_individual_sum <- 
  etn_dataset %>% 
  #dplyr::filter(datetime %>% between(start, end)) %>%
  dplyr::mutate(monthyear = date_time %>% floor_date(unit = 'months'))%>%
  # get out total n of detection for that month
  dplyr::group_by(monthyear) %>%
  dplyr::mutate(n_detections_monthyear = n())%>%
  dplyr::ungroup() %>%
  ### get out total n of detections per station per month
  dplyr::group_by(monthyear, station_name) %>%
  dplyr::mutate(n_detections_monthyear_station = n())%>%
  dplyr::ungroup() %>%
  # dplyr::mutate(date = datetime %>% as.Date()) %>% # for STAC dataset
  dplyr::group_by(monthyear, station_name, tag_serial_number) %>%
  dplyr::summarise(
    # latitude = mean(latitude, na.rm = T),
    # longitude = mean(longitude, na.rm = T),
    deploy_latitude = mean(deploy_latitude, na.rm = T),
    deploy_longitude = mean(deploy_longitude, na.rm = T),
    n_detections = n(),
    n_detections_monthyear = n_detections_monthyear %>% unique(), # %>% paste0(collapse = ", "),
    n_detections_monthyear_station = n_detections_monthyear_station %>% unique(), # %>% paste0(collapse = ", ")
    # , tag_serial_nums = paste0(tag_serial_number %>% unique(), collapse = ", ") 
  ) 

# save as .RDS
etn_monthyear_individual_sum_path <- "./data/etn_sum_seabass_monthyear_indivdual.rds"
saveRDS(etn_monthyear_individual_sum, etn_monthyear_individual_sum_path)

# appending row to wms_registry
wms_registry <- add_row(
  wms_registry,
  env_data_name = "seabass acoustic detections",
  collection_name = etn_collection,
  wms_link    = etn_dataset_href,
  wms_base    = etn_daily_sum_path,
  wms_layer_name  = "",
  legend_link = "",
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj
rm(etn_items, etn_dataset, etn_dataset_href, href, title, etn_daily_sum_path, etn_collection, dataset_name, etn_daily_sum, feature)

## Offshore Wind Farm -------------------------------------------------------

c_owf_list <- c_all %>% dplyr::filter(grepl("wind", c_all$value))
c_owf_selection <- c_owf_list$value[10]

owf_items <- stac_obj %>%
  rstac::stac_search(collections = c_owf_selection, limit = 500) %>%
  rstac::get_request() %>%
  rstac::items_fetch()

# wms
owf_wms_link <- owf_items$features[[1]]$assets$wms$href
owf_wms_base <- sub("\\?.*$", "", owf_wms_link)
owf_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", owf_wms_link)


## legend
owf_legend_url <- paste0(
  owf_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", owf_layer_name,
  "&VERSION=1.1.1"
)


# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = owf_wms_base,
#     layers  = owf_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">Wind farms</div>',
#       '<img src="', owf_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry
wms_registry <- add_row(
  wms_registry,
  env_data_name = "owf",
  collection_name = c_owf_selection,
  wms_link    = owf_wms_link,
  wms_base    = owf_wms_base,
  wms_layer_name  = owf_layer_name,
  legend_link = owf_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj we no longer need
rm(c_owf_list, owf_items, owf_layer_name, owf_wms_link, c_owf_selection, owf_wms_base, owf_legend_url)

## Submarine Power Cables (spc) -------------------------------------------------------

c_spc_list <- c_all %>% dplyr::filter(grepl("cables", c_all$value))
c_spc_selection <- c_spc_list$value[1]

spc_items <- stac_obj %>%
  rstac::stac_search(collections = c_spc_selection, limit = 500) %>%
  rstac::get_request() %>%
  rstac::items_fetch()

# wms
spc_wms_link <- spc_items$features[[2]]$assets$wms$href
spc_wms_base <- sub("\\?.*$", "", spc_wms_link)
spc_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", spc_wms_link)


## legend
spc_legend_url <- paste0(
  spc_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", spc_layer_name,
  "&VERSION=1.1.1"
)


# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = spc_wms_base,
#     layers  = spc_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">SPC</div>',
#       '<img src="', spc_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry
wms_registry <- add_row(
  wms_registry,
  env_data_name = "spc",
  collection_name = c_spc_selection,
  wms_link    = spc_wms_link,
  wms_base    = spc_wms_base,
  wms_layer_name  = spc_layer_name,
  legend_link = spc_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj we no longer need
rm(c_spc_list, spc_items, spc_layer_name, spc_wms_link, c_spc_selection, spc_wms_base, spc_legend_url)


## marine spatial plan -----------------------------------------------------

c_msp_list <- c_all %>% dplyr::filter(grepl("spatial_planning", c_all$value))
c_msp_selection <- c_msp_list$value[1]

msp_items <- stac_obj %>%
  rstac::stac_search(collections = c_msp_selection, limit = 500) %>%
  rstac::get_request() %>%
  rstac::items_fetch()

# wms
msp_wms_link <- msp_items$features[[3]]$assets$wms$href
msp_wms_base <- sub("\\?.*$", "", msp_wms_link)
msp_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", msp_wms_link)


## legend
msp_legend_url <- paste0(
  msp_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", msp_layer_name,
  "&VERSION=1.1.1"
)

# browseURL(msp_legend_url)

# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = msp_wms_base,
#     layers  = msp_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">Spatial Planning Areas</div>',
#       '<img src="', msp_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry

wms_registry <- add_row(
  wms_registry,
  env_data_name = "msp",
  collection_name = c_msp_selection,
  wms_link    = msp_wms_link,
  wms_base    = msp_wms_base,
  wms_layer_name  = msp_layer_name,
  legend_link = msp_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj we no longer need
rm(c_msp_list, msp_layer_name, msp_wms_link, msp_wms_base, msp_legend_url)


## some EEZs -------------------------------------------

stac_eez_wms_link <- msp_items$features[[4]]$assets$wms$href
stac_eez_wms_base <- sub("\\?.*$", "", stac_eez_wms_link)
stac_eez_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", stac_eez_wms_link)


## legend
stac_eez_legend_url <- paste0(
  stac_eez_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", stac_eez_layer_name,
  "&VERSION=1.1.1"
)

# browseURL(stac_eez_legend_url)

# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = stac_eez_wms_base,
#     layers  = stac_eez_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">stac_eez Areas</div>',
#       '<img src="', stac_eez_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry

wms_registry <- add_row(
  wms_registry,
  env_data_name = "stac_eez",
  collection_name = c_msp_selection,
  wms_link    = stac_eez_wms_link,
  wms_base    = stac_eez_wms_base,
  wms_layer_name  = stac_eez_layer_name,
  legend_link = stac_eez_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj we no longer need
rm(msp_items, stac_eez_layer_name, stac_eez_wms_link, stac_eez_wms_base, c_msp_selection, stac_eez_legend_url)

## Natura2000 areas -----------------------------------------------------

c_natura2000_list <- c_all %>% dplyr::filter(grepl("protected_areas", c_all$value))
c_natura2000_selection <- c_natura2000_list$value[1]

natura2000_items <- stac_obj %>%
  rstac::stac_search(collections = c_natura2000_selection, limit = 500) %>%
  rstac::get_request() %>%
  rstac::items_fetch()

# wms
natura2000_wms_link <- natura2000_items$features[[4]]$assets$wms$href
natura2000_wms_base <- sub("\\?.*$", "", natura2000_wms_link)
natura2000_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", natura2000_wms_link)


## legend
natura2000_legend_url <- paste0(
  natura2000_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", natura2000_layer_name,
  "&VERSION=1.1.1"
)

# browseURL(natura2000_legend_url)

# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = natura2000_wms_base,
#     layers  = natura2000_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">Natura2000 Areas</div>',
#       '<img src="', natura2000_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry

wms_registry <- add_row(
  wms_registry,
  env_data_name = "natura2000",
  collection_name = c_natura2000_selection,
  wms_link    = natura2000_wms_link,
  wms_base    = natura2000_wms_base,
  wms_layer_name  = natura2000_layer_name,
  legend_link = natura2000_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj we no longer need
rm(c_natura2000_list, natura2000_layer_name, natura2000_wms_link, natura2000_wms_base, natura2000_legend_url)

## international sea conventions -------------------------------------------

sea_conventions_wms_link <- natura2000_items$features[[3]]$assets$wms$href
sea_conventions_wms_base <- sub("\\?.*$", "", sea_conventions_wms_link)
sea_conventions_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", sea_conventions_wms_link)


## legend
sea_conventions_legend_url <- paste0(
  sea_conventions_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", sea_conventions_layer_name,
  "&VERSION=1.1.1"
)

# browseURL(sea_conventions_legend_url)

# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = sea_conventions_wms_base,
#     layers  = sea_conventions_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">sea_conventions Areas</div>',
#       '<img src="', sea_conventions_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry

wms_registry <- add_row(
  wms_registry,
  env_data_name = "sea_conventions",
  collection_name = c_natura2000_selection,
  wms_link    = sea_conventions_wms_link,
  wms_base    = sea_conventions_wms_base,
  wms_layer_name  = sea_conventions_layer_name,
  legend_link = sea_conventions_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj we no longer need
rm(natura2000_items, sea_conventions_layer_name, sea_conventions_wms_link, sea_conventions_wms_base, c_natura2000_selection, sea_conventions_legend_url)


## bathymetry --------------------------------------------------------------

c_bathy_list <- c_all %>% dplyr::filter(grepl("elevation", c_all$value))
c_bathy_selection <- c_bathy_list$value[1]

bathy_objects <- stac_obj %>%
  rstac::collections(c_bathy_selection)%>%
  rstac::get_request()

bathy_items <- stac_obj %>%
  rstac::stac_search(collections = c_bathy_selection) %>%
  rstac::get_request()%>%
  rstac::items_fetch()

bathy_wms_link <- bathy_items$features[[8]]$assets$wms$href
bathy_wms_base <- sub("\\?.*$", "", bathy_wms_link)
bathy_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", bathy_wms_link)
bathy_layer_name_1 <- "mean_atlas_land"
bathy_layer_name_2 <- "emodnet:mean_multicolour"

## legend
bathy_legend_url_1 <- paste0(
  bathy_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", bathy_layer_name_1,
  "&VERSION=1.1.1"
)

bathy_legend_url_2 <- paste0(
  bathy_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", bathy_layer_name_2,
  "&VERSION=1.1.1"
)

# browseURL(bathy_legend_url_1)

# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = bathy_wms_base,
#     layers  = bathy_layer_name_1,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">Bathymetry</div>',
#       '<img src="', bathy_legend_url_1, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# add both bathymetry layers
wms_registry <- add_row(
  wms_registry,
  env_data_name = "bathy_atlas", # more 'elegant' looking
  collection_name = c_bathy_selection,
  wms_link    = bathy_wms_link,
  wms_base    = bathy_wms_base,
  wms_layer_name  = bathy_layer_name_1,
  legend_link = bathy_legend_url_1,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

wms_registry <- add_row(
  wms_registry,
  env_data_name = "bathy_multicolor", # more intense color scale
  collection_name = c_bathy_selection,
  wms_link    = bathy_wms_link,
  wms_base    = bathy_wms_base,
  wms_layer_name  = bathy_layer_name_2,
  legend_link = bathy_legend_url_2,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj we no longer need
rm(c_bathy_list, bathy_items, bathy_objects, bathy_layer_name_1, bathy_layer_name_2, bathy_wms_link, c_bathy_selection, bathy_wms_base, bathy_legend_url_1, bathy_legend_url_2)


## shipwrecks_emodnet -------------------------------------------------------

c_shipwrecks_emodnet_list <- c_all %>% dplyr::filter(grepl("heritage", c_all$value))
c_shipwrecks_emodnet_selection <- c_shipwrecks_emodnet_list$value[1]

shipwrecks_emodnet_items <- stac_obj %>%
  rstac::stac_search(collections = c_shipwrecks_emodnet_selection, limit = 500) %>%
  rstac::get_request() %>%
  rstac::items_fetch()

# wms
shipwrecks_emodnet_wms_link <- shipwrecks_emodnet_items$features[[4]]$assets$wms$href
shipwrecks_emodnet_wms_base <- sub("\\?.*$", "", shipwrecks_emodnet_wms_link)
shipwrecks_emodnet_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", shipwrecks_emodnet_wms_link)


## legend
shipwrecks_emodnet_legend_url <- paste0(
  shipwrecks_emodnet_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", shipwrecks_emodnet_layer_name,
  "&VERSION=1.1.1"
)


# # test map
# leaflet() %>%
#   setView(3, 51.5, zoom = 8) %>%
#   addTiles() %>%
#   addWMSTiles(
#     baseUrl = shipwrecks_emodnet_wms_base,
#     layers  = shipwrecks_emodnet_layer_name,
#     options = WMSTileOptions(
#       format = "image/png",
#       transparent = T,
#       opacity = 1
#     )) %>%
#   addControl(
#     html = paste0(
#       '<div style="background:white;padding:6px;border-radius:4px;">',
#       '<div style="font-weight:600;margin-bottom:4px;">Wind farms</div>',
#       '<img src="', shipwrecks_emodnet_legend_url, '" />',
#       '</div>'
#     ),position = "bottomright"
#   )

# appending row to wms_registry
wms_registry <- add_row(
  wms_registry,
  env_data_name = "shipwrecks_emodnet",
  collection_name = c_shipwrecks_emodnet_selection,
  wms_link    = shipwrecks_emodnet_wms_link,
  wms_base    = shipwrecks_emodnet_wms_base,
  wms_layer_name  = shipwrecks_emodnet_layer_name,
  legend_link = shipwrecks_emodnet_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj we no longer need
rm(c_shipwrecks_emodnet_list, shipwrecks_emodnet_items, shipwrecks_emodnet_layer_name, shipwrecks_emodnet_wms_link, c_shipwrecks_emodnet_selection, shipwrecks_emodnet_wms_base, shipwrecks_emodnet_legend_url)


## seabedhabitats -doesnt work -----------------------------------------------------
# 
# c_seabedhabitats_list <- c_all %>% dplyr::filter(grepl("emodnet-seabed_habitats", c_all$value))
# c_seabedhabitats_selection <- c_seabedhabitats_list$value[1]
# c_seabedhabitats_selection
# # #12 # should be the layer
# # # 4 shows sth
# # # 5 # gives complete legend: "https://ows.emodnet-seabedhabitats.eu/geoserver/emodnet_view/wms?SERVICE=WMS&REQUEST=GetLegendGraphic&FORMAT=image/png&LAYER=eunismaps_all&VERSION=1.1.1"
# # # 13 shows something but not BPNS
# # # 15, 16, 23 only baltic
# # # 22 IRL
# # # 24 # emodnet-modelled_projections_of_habitat_for_commercial_fish_around_north_western_europe_under_climate_change_2020_to_2060
# # # 27 seagrass cover (EOV)
# # 
# seabedhabitats_items <- stac_obj %>%
#   rstac::stac_search(collections = c_seabedhabitats_selection, limit = 500) %>%
#   rstac::get_request() %>%
#   rstac::items_fetch()
# # 
# # 
# # wms
seabedhabitats_wms_link <- "https://ows.emodnet-seabedhabitats.eu/geoserver/emodnet_view/wms?SERVICE=WMS&REQUEST=GetMap&LAYERS=eusm_eunis2019_group&VERSION=1.3.0&CRS=CRS:84&BBOX=-180,-90,180,90&WIDTH=800&HEIGHT=600&FORMAT=image/png" #seabedhabitats_items$features[[1]]$assets$wms$href
# seabedhabitats_wms_link
# browseURL(seabedhabitats_wms_link)
seabedhabitats_wms_base <- sub("\\?.*$", "", seabedhabitats_wms_link)
seabedhabitats_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", seabedhabitats_wms_link)

# # parquet
# seabedhabitats_parquet_link <- seabedhabitats_items$features[[1]]$assets$parquet$href
# seabedhabitats_parquet <- arrow::read_parquet(seabedhabitats_parquet_link, format = "parquet")
# # 
# # library(sf)
# # #library(dplyr)
# # geom_test <- st_as_sfc(as.list(seabedhabitats_parquet$geometry[1:5]), crs = 4326)
# # st_bbox(geom_test)
# # 
# # library(DBI)
# # library(duckdb)
# # 
# # con <- dbConnect(duckdb())
# # 
# # dbExecute(con, "INSTALL spatial;")
# # dbExecute(con, "LOAD spatial;")
# # 
# # sql <- "
# # SELECT *
# # FROM read_parquet($1)
# # WHERE ST_Intersects(
# #   geometry,
# #   ST_MakeEnvelope(2, 50, 6, 52)
# # )
# # "
# # 
# # subset_df <- dbGetQuery(con, sql, params = list(seabedhabitats_parquet_link))
# # subset_df <- dbGetQuery(con, sql, params = list(seabedhabitats_parquet_link))
# # 
# # dbDisconnect(con, shutdown = TRUE)
# # 
# 
# #library(sf)
# #seabedhabitats_p_sf <- sf::st_as_sf(seabedhabitats_parquet)
# 
# 
## legend
seabedhabitats_legend_url <- paste0(
  seabedhabitats_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", seabedhabitats_layer_name,
  "&VERSION=1.1.1"
)

# browseURL(seabedhabitats_legend_url)
# 
# test map
leaflet() %>%
  setView(3, 51.5, zoom = 8) %>%
  addTiles() %>%
  addWMSTiles(
    baseUrl = seabedhabitats_wms_base,
    layers  = seabedhabitats_layer_name,
    options = WMSTileOptions(
      format = "image/png",
      transparent = T,
      opacity = 1
    )) %>%
  addControl(
    html = paste0(
      '<div style="background:white;padding:6px;border-radius:4px;">',
      '<div style="font-weight:600;margin-bottom:4px;">seabedhabitats Areas</div>',
      '<img src="', seabedhabitats_legend_url, '" />',
      '</div>'
    ),position = "bottomright"
  )


# appending row to wms_registry
wms_registry <- add_row(
  wms_registry,
  env_data_name = "seabedhabitats",
  collection_name = "",
  wms_link    = seabedhabitats_wms_link,
  wms_base    = seabedhabitats_wms_base,
  wms_layer_name  = seabedhabitats_layer_name,
  legend_link = seabedhabitats_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj we no longer need
rm(seabedhabitats_layer_name, seabedhabitats_wms_link, seabedhabitats_wms_base, seabedhabitats_legend_url)



# non-STAC (for the moment) layers ----------------------------------------
## a few layers are not in the EDITO STAC catalogue so we call them externally

## EEZ ---------------------------------------------------------------------

wms_registry <- add_row(
  wms_registry,
  env_data_name = "eez", 
  collection_name = "",
  wms_link    = "https://geo.vliz.be/geoserver/MarineRegions/wms?service=WMS&version=1.1.0&request=GetMap&layers=MarineRegions%3Aeez&bbox=-180.0%2C-62.78834217149148%2C180.0%2C86.99400535016684&width=768&height=330&srs=EPSG%3A4326&styles=&format=application/openlayers",
  wms_base    = "https://geo.vliz.be/geoserver/MarineRegions/wms",
  wms_layer_name  = "eez",
  legend_link = "",
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 


## BPNS shipwrecks --------------------------------------------------------------

wms_registry <- add_row(
  wms_registry,
  env_data_name = "shipwrecks",
  collection_name = "",
  wms_link    = "https://geo.vliz.be/geoserver/Kustportaal/wms?service=WMS&version=1.1.0&request=GetMap&layers=Kustportaal%3Ascheepswrakken_20180604&bbox=2.305333333%2C51.11496667%2C4.39055%2C51.84788333&width=768&height=330&srs=EPSG%3A4326&styles=&format=application/openlayers",
  wms_base    = "https://geo.vliz.be/geoserver/Kustportaal/wms",
  wms_layer_name  = "Kustportaal:scheepswrakken_20180604",
  legend_link = "",
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

## BPNS seabed substrates --------------------------------------------------------------
# for now from the RBINS geoserver: https://spatial.naturalsciences.be/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?0&filter=false

# wms
seabedsubstrates_wms_link <- "https://spatial.naturalsciences.be/geoserver/od_nature/wms?service=WMS&version=1.1.0&request=GetMap&layers=od_nature%3Asubstrate_BPNS_Seafloor&bbox=3783778.2435288355%2C3135783.371861253%2C3859575.3740788964%2C3222401.382971901&width=672&height=768&srs=EPSG%3A3035&styles=&format=application/openlayers"
seabedsubstrates_wms_base <- "https://spatial.naturalsciences.be/geoserver/od_nature/wms"
seabedsubstrates_layer_name <- "od_nature:seabed_substrate_map"

## legend
seabedsubstrates_legend_url <- paste0(
  seabedsubstrates_wms_base,
  "?SERVICE=WMS&REQUEST=GetLegendGraphic",
  "&FORMAT=image/png",
  "&LAYER=", seabedsubstrates_layer_name,
  "&VERSION=1.1.1"
)

wms_registry <- add_row(
  wms_registry,
  env_data_name = "seabedsubstrates",
  collection_name = "",
  wms_link    = seabedsubstrates_wms_link,
  wms_base    = seabedsubstrates_wms_base,
  wms_layer_name  = seabedsubstrates_layer_name,
  legend_link = seabedsubstrates_legend_url,
  .added_at        = Sys.time()) %>%
  arrange(desc(.added_at)) %>%
  distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 

# remove obj we no longer need
rm(seabedsubstrates_layer_name, seabedsubstrates_wms_link, seabedsubstrates_wms_base, seabedsubstrates_legend_url)

# SST - TODO --------------------------------------------------------------

c_sst_list <- c_all %>% dplyr::filter(grepl("sea_surface_foundation_temperature", c_all$value))
c_sst_selection <- c_sst_list$value[1]

sst_items <- stac_obj %>%
  rstac::stac_search(collections = c_sst_selection, limit = 500) %>%
  rstac::get_request() %>%
  rstac::items_fetch()

sst_selected_item <- sst_items$features[[37]]
sst_selected_item


## wmts --------------------------------------------------------------------
# 
# sst_wms_link <- sst_selected_item$assets$wmts$href
# sst_wms_link #%>% browseURL()
# 
# parse_wmts_href <- function(href) {
#   u <- xml2::url_parse(href)
#   q <- u$query
#   
#   # base endpoint = scheme + host + path (without query)
#   base <- paste0(u$scheme, "://", u$server, u$path)
#   
#   layer <- sub("^.*(?:^|&)?layer=([^&]+).*$", "\\1", u$query)
#   if (identical(layer, u$query)) stop("Could not find 'layer=' in query: ", u$query)
#   
#   list(base = base, layer = layer)
# }
# 
# wmts_info <- parse_wmts_href(sst_wms_link)
# wmts_base  <- wmts_info$base
# wmts_layer <- wmts_info$layer
# 
# wmts_base
# wmts_layer
# 
# get_wmts_metadata <- function(base, layer_id) {
#   cap_url <- paste0(base, "?SERVICE=WMTS&REQUEST=GetCapabilities")
#   cap_xml <- read_xml(cap_url)
#   ns <- xml_ns(cap_xml)
#   
#   layer_node <- xml_find_first(
#     cap_xml,
#     paste0(".//wmts:Layer[ows:Identifier='", layer_id, "']"),
#     ns
#   )
#   if (is.na(layer_node)) stop("Layer not found in GetCapabilities: ", layer_id)
#   
#   # tile matrix sets for this layer
#   tms <- xml_text(xml_find_all(layer_node, ".//wmts:TileMatrixSetLink/wmts:TileMatrixSet", ns))
#   tms <- unique(tms)
#   
#   # try to pick a Leaflet-friendly one if present
#   tms_pick <- if ("GoogleMapsCompatible" %in% tms) "GoogleMapsCompatible" else tms[1]
#   
#   # dimension names (often includes Time/TIME)
#   dim_names <- xml_text(xml_find_all(layer_node, ".//wmts:Dimension/ows:Identifier", ns))
#   
#   # values are sometimes huge; we’ll just keep a few
#   dim_values <- xml_text(xml_find_all(layer_node, ".//wmts:Dimension/wmts:Value", ns))
#   
#   list(
#     cap_url = cap_url,
#     tms_available = tms,
#     tms_pick = tms_pick,
#     dim_names = dim_names,
#     dim_values_head = head(dim_values, 20)
#   )
# }
# 
# wmts_meta <- get_wmts_metadata(wmts_base, wmts_layer)
# 
# wmts_meta$tms_available
# wmts_meta$tms_pick
# wmts_meta$dim_names
# wmts_meta$dim_values_head

# 
# # wms
# sst_wms_link <- sst_items$features[[2]]$assets$wms$href
# sst_wms_base <- sub("\\?.*$", "", sst_wms_link)
# sst_layer_name <- sub(".*LAYERS=([A-Za-z0-9_:]+).*", "\\1", sst_wms_link)
# 
# 
# ## legend
# sst_legend_url <- paste0(
#   sst_wms_base,
#   "?SERVICE=WMS&REQUEST=GetLegendGraphic",
#   "&FORMAT=image/png",
#   "&LAYER=", sst_layer_name,
#   "&VERSION=1.1.1"
# )
# 
# 
# # # test map
# # leaflet() %>%
# #   setView(3, 51.5, zoom = 8) %>%
# #   addTiles() %>%
# #   addWMSTiles(
# #     baseUrl = sst_wms_base,
# #     layers  = sst_layer_name,
# #     options = WMSTileOptions(
# #       format = "image/png",
# #       transparent = T,
# #       opacity = 1
# #     )) %>%
# #   addControl(
# #     html = paste0(
# #       '<div style="background:white;padding:6px;border-radius:4px;">',
# #       '<div style="font-weight:600;margin-bottom:4px;">Wind farms</div>',
# #       '<img src="', sst_legend_url, '" />',
# #       '</div>'
# #     ),position = "bottomright"
# #   )
# 
# # appending row to wms_registry
# wms_registry <- add_row(
#   wms_registry,
#   env_data_name = "sst",
#   collection_name = c_sst_selection,
#   wms_link    = sst_wms_link,
#   wms_base    = sst_wms_base,
#   wms_layer_name  = sst_layer_name,
#   legend_link = sst_legend_url,
#   .added_at        = Sys.time()) %>%
#   arrange(desc(.added_at)) %>%
#   distinct(env_data_name, wms_link, wms_layer_name, .keep_all = TRUE) 
# 
# # remove obj we no longer need
# rm(c_sst_list, sst_items, sst_layer_name, sst_wms_link, c_sst_selection, sst_wms_base, sst_legend_url)
# 


# save wms registry -------------------------------------------------------
wms_registry_path <- "./data/EDITO_STAC_layers_metadata.csv"
write.csv(wms_registry, wms_registry_path)

#rm(list = ls())

##################################################################################
##################################################################################

# Author: Lotte Pohl
# Email: lotte.pohl@vliz.be
# Date: 2026-01-29
# Script Name: ~/DUC2_viewer_acoustic_telemetry/helpers/load_acoustic_telemetry_GAM_s3.R
# Script Description: load data layers from s3 bucket resulting from GAM modeling workflow

##################################################################################
##################################################################################


# install and load libraries ----------------------------------------------
library(terra)
library(httr)

# -----------------------
# List of layers
# -----------------------
prediction_layers_info <- list(
  "Predictions inside OWF"        = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/predictions_inside_owf_median_months.nc",monthly = TRUE),
  "Predictions outside OWF"       = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/predictions_outside_owf_median_months.nc", monthly = TRUE),
  "Diff OWF"            = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/diff_owf.nc", monthly = TRUE)
)

env_layers_info <- list(
  "Bathymetry"          = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/bathy.nc", monthly = FALSE),
  "Habitats"            = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/habitats.nc", monthly = FALSE),
  "LOD median months"   = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/lod_median_months.nc", monthly = TRUE),
  "OWF distance"        = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/owf_dist.nc", monthly = FALSE),
  "Shipwreck distance"  = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/shipwreck_dist.nc", monthly = FALSE),
  "SST median months"   = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/sst_median_months.nc", monthly = TRUE),
  "X coords"            = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/x_m_4326.nc", monthly = FALSE),
  "Y coords"            = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/y_m_4326.nc", monthly = FALSE)
)

data_layers_info <- list(
  "Counts median months"        = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/counts_median_months.nc",monthly = TRUE),
  "N active tags median months" = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/n_active_tags_median_months.nc", monthly = TRUE)
)

# -----------------------
# Load layers at startup
# -----------------------
load_layer <- function(url) {
  tf <- tempfile(fileext = ".nc")
  GET(url, write_disk(tf, overwrite = TRUE))
  terra::rast(tf)
}

prediction_layers <- lapply(prediction_layers_info, function(x) load_layer(x$url))
env_layers <- lapply(env_layers_info, function(x) load_layer(x$url))
data_layers <- lapply(data_layers_info, function(x) load_layer(x$url))

# -----------------------
# Create palettes for layers
# -----------------------
prediction_palettes <- lapply(prediction_layers, function(r) {
  colorNumeric("viridis", values(r), na.color = "transparent")
})

env_palettes <- lapply(env_layers, function(r) {
  colorNumeric("viridis", values(r), na.color = "transparent")
})

data_palettes <- lapply(data_layers, function(r) {
  colorNumeric("viridis", values(r), na.color = "transparent")
})

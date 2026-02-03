
load_etn_dataset <- function(wms_layers, key = "seabass acoustic detections") {
  # TODO: replace with JHN's function to read in an etn dataset
  arrow::read_parquet(wms_layers[[key]]$wms_link)
}

summarise_etn_monthyear_individual <- function(etn_dataset) {
  etn_dataset %>%
    dplyr::mutate(monthyear = lubridate::floor_date(datetime, unit = "months")) %>%
    
    # totals per month
    dplyr::group_by(monthyear) %>%
    dplyr::mutate(n_detections_monthyear = dplyr::n()) %>%
    dplyr::ungroup() %>%
    
    # totals per month x station
    dplyr::group_by(monthyear, station_name) %>%
    dplyr::mutate(n_detections_monthyear_station = dplyr::n()) %>%
    dplyr::ungroup() %>%
    
    # individual counts per month x station x tag
    dplyr::group_by(monthyear, station_name, tag_serial_number) %>%
    dplyr::summarise(
      deploy_latitude  = mean(latitude,  na.rm = TRUE),
      deploy_longitude = mean(longitude, na.rm = TRUE),
      n_detections = dplyr::n(),
      n_detections_monthyear = dplyr::first(n_detections_monthyear),
      n_detections_monthyear_station = dplyr::first(n_detections_monthyear_station),
      .groups = "drop"
    )
}

build_monthyear_rds <- function(
    out_path = "data/etn_sum_seabass_monthyear_individual.rds",
    wms_layers = NULL,
    metadata_fun = load_STAC_metadata,
    dataset_key = "seabass acoustic detections"
) {
  if (is.null(wms_layers)) wms_layers <- metadata_fun()
  
  # etn_dataset <- load_etn_dataset(wms_layers, key = dataset_key)
  
  # if STAC etn data are unavailable
  etn_dataset <- 
    readRDS("./data/detections.rds") %>%
      dplyr::rename(latitude = deploy_latitude,
                    longitude = deploy_longitude,
                    datetime = date_time)
  
  etn_sum <- summarise_etn_monthyear_individual(etn_dataset)
  
  # checks for dublicates
  stopifnot(!anyDuplicated(etn_sum[c("monthyear","station_name","tag_serial_number")]))
  
  
  saveRDS(etn_sum, out_path)
  invisible(etn_sum)
}


# preapre minicharts leaflet inputs

prep_minicharts_inputs <- function(deployments, etn_monthyear_individual_sum) {
  
  # stations: ensure exactly 1 row per station_name
  stations <- deployments %>%
    dplyr::group_by(station_name) %>%
    dplyr::summarise(
      lat = mean(deploy_latitude,  na.rm = TRUE),
      lon = mean(deploy_longitude, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::distinct(station_name, lon, lat) %>%
    dplyr::arrange(station_name)
  
  # detections wide (already unique per monthyear/station/tag from your stopifnot)
  detections_wide <- etn_monthyear_individual_sum %>%
    dplyr::transmute(
      month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01")),
      station_name,
      tag_serial_number = paste0("id_", tag_serial_number),
      n_detections
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(month, station_name),
      names_from  = tag_serial_number,
      values_from = n_detections,
      values_fill = 0,
      values_fn   = sum  # safety: if duplicates exist, sum them
    )
  
  # detections_wide <- etn_monthyear_individual_sum %>%
  #   dplyr::mutate(month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01"))) %>%
  #   tidyr::pivot_wider(
  #     names_from  = tag_serial_number,
  #     values_from = n_detections,
  #     names_prefix = "id_",
  #     values_fill  = 0
  #   )
  
  ids <- names(detections_wide %>% dplyr::select(dplyr::starts_with("id_")))
  months <- sort(unique(detections_wide$month))
  
  anim_df <- tidyr::expand_grid(
    month = months,
    station_name = stations$station_name
  ) %>%
    dplyr::left_join(detections_wide, by = c("month", "station_name")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(ids), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::arrange(month, station_name) %>%
    dplyr::left_join(stations, by = "station_name")
  
  # size scaling
  station_month_totals <- etn_monthyear_individual_sum %>%
    dplyr::mutate(month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01"))) %>%
    dplyr::group_by(month, station_name) %>%
    dplyr::summarise(n_station = sum(n_detections, na.rm = TRUE), .groups = "drop")
  
  month_totals <- station_month_totals %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(n_month = sum(n_station, na.rm = TRUE), .groups = "drop")
  
  anim_df <- anim_df %>%
    dplyr::left_join(station_month_totals, by = c("month", "station_name")) %>%
    dplyr::left_join(month_totals, by = "month") %>%
    dplyr::mutate(
      n_station = tidyr::replace_na(n_station, 0),
      n_month   = pmax(tidyr::replace_na(n_month, 0), 1),
      rel = n_station / n_month,
      pie_size = 12 + 80 * sqrt(rel)
    )
  
  width_all  <- anim_df$pie_size
  height_all <- anim_df$pie_size
  
  # integrity checks (catch your “row mismatch” early)
  stopifnot(
    nrow(anim_df) == length(anim_df$month),
    nrow(anim_df) == length(width_all),
    nrow(anim_df) == length(height_all),
    nrow(anim_df) == length(anim_df$station_name),
    all(stations$station_name %in% anim_df$station_name)
  )
  
  list(
    stations = stations,
    anim_df = anim_df,
    ids = ids,
    months = months,
    width_all = width_all,
    height_all = height_all
  )
}

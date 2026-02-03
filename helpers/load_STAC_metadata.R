load_STAC_metadata <- function(metadata_csv = "data/EDITO_STAC_layers_metadata.csv",
                            edito_script = "helpers/_EDITO_STAC_data_access.R") {
  
  if (!file.exists(metadata_csv)) {
    source(edito_script, local = TRUE)
  }
  
  read.csv(metadata_csv) %>%
    base::split(.$env_data_name)
}
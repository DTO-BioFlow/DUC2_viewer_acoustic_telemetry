source_all <- function(path = "R", pattern = "\\.R$", ignore_prefix = "^_") {
  files <- list.files(path, pattern = pattern, full.names = TRUE, recursive = TRUE)
  
  # ignore files starting with "_" (common convention for "do not autoload")
  files <- files[!grepl(paste0("/", ignore_prefix), files)]
  
  # source in a stable order
  files <- sort(files)
  
  invisible(lapply(files, function(f) source(f, local = FALSE)))
}

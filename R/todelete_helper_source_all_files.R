# source_all <- function(path = "R", pattern = "\\.R$") {
#   files <- list.files(path, pattern = pattern, full.names = TRUE, recursive = TRUE)
#   
#   # ignore files starting with "_" by checking the filename only
#   files <- files[!startsWith(basename(files), "_")]
#   
#   files <- sort(files)
#   invisible(lapply(files, function(f) source(f, local = FALSE)))
# }

#!/bin/bash
# init.sh - install R packages required for the Shiny app

# Set CRAN mirror to avoid prompts
Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org'))"

# Install required packages
Rscript -e "install.packages(c('shiny', 'leaflet', 'terra', 'httr', 'arrow', 'rstac', 'purrr', 'dplyr'))"

echo "R packages installed successfully."

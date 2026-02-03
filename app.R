

library(shiny)
library(DT)
library(leaflet)
library(glue)
library(terra)
library(htmltools)
library(leaflet.minicharts)
library(tidyr)
library(RColorBrewer)
library(rstac)
library(purrr)
library(arrow)
library(dplyr)
library(lubridate)

# load data ---------------------------------------------------------------
# TODO: outsource into a helper script?
deployments <- readRDS("data/deployments.rds")
etn_monthyear_individual_sum <- readRDS("data/etn_sum_seabass_monthyear_individual.rds")


# source scripts ----------------------------------------------------------
# we first source all .R files in the ./R folder in this repo
source("helpers/source_all_files.R")
source_all(path = "R")
# source_all(path = "helpers") #probably not needed in the end
source("./helpers/load_acoustic_telemetry_GAM_s3.R")
source("./helpers/load_STAC_metadata.R")
# get STAC metadata
wms_layers <- load_STAC_metadata()
source("./helpers/wrangle_acoustic_telemetry_data.R")


# ui ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML(glue::glue("
      :root {{
        --blue-light: {blue_light};
        --blue-medium: {blue_medium};
        --blue-dark: {blue_dark};
      }}
    "))),
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  
  titlePanel("DUC2: Impact from offshore infrastructures on marine life"),
  
  mainPanel(
    width = 12,
    tags$div(
      class = "top-tabs container-fluid",
      
      tags$a(
        href = bioflow_url,
        target = "_blank",
        rel = "noopener",
        class = "top-tabs-logo",
        tags$img(
          src = "Logo_BIO-Flow2023_Final_Positive.png",
          height = "42px",
          alt = "DTO-Bioflow"
        )
      ),
      
      tabsetPanel(
        id = "tabsetPanelID",
        type = "tabs",
        
        tabPanel("Home", style = "font-size: 16px;", mod_home_ui("home")),
        
        tabPanel(
          title = tagList(
            tags$img(src = "D_labrax_phylopic_CC0.png", height = "24px",
                     style = "vertical-align:middle; margin-right:8px;"),
            tags$span("European seabass", style = "font-size: 16px; vertical-align:middle;")
          ),
          class = "lower-level-tabs",
          mod_seabass_ui("seabass")
        ),
        
        tabPanel(
          title = tagList(
            tags$img(src = "P_phocoena_phylopic_CC0.png", height = "24px",
                     style = "vertical-align:middle; margin-right:8px;"),
            tags$span("Harbour porpoise", style = "font-size: 16px; vertical-align:middle;")
          ),
          class = "lower-level-tabs",
          mod_porpoise_ui("porpoise")
        )
      )
    )
  )
)


# server ------------------------------------------------------------------
server <- function(input, output, session) {
  mod_home_server("home")
  mod_seabass_server("seabass")
  mod_porpoise_server("porpoise")
}

shinyApp(ui, server)

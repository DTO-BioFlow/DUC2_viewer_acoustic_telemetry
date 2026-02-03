# TODO: put minicharts reactive maps from app_tests.R

mod_seabass_telemetry_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("data_map"), height = 700),
    DTOutput(ns("acoustic_telemetry"))
  )
}

mod_seabass_telemetry_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$data_map <- renderLeaflet({ make_base_map() })
    output$acoustic_telemetry <- renderDT({ datasets::cars })
  })
}

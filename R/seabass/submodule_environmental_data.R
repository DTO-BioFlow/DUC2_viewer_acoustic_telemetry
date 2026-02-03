mod_seabass_env_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("env_map"), height = 700)
}

mod_seabass_env_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$env_map <- renderLeaflet({
      make_env_wms_map(base_map = make_base_map(), wms_layers = wms_layers)
    })
  })
}

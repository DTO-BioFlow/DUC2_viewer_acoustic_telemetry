mod_seabass_env_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("env_map"), height = 600)
}

mod_seabass_env_server <- function(id, wms_layers = wms_layers, base_map_fun = make_base_map, env_map_fun = make_env_wms_map) {
  moduleServer(id, function(input, output, session) {
    output$env_map <- renderLeaflet({
      env_map_fun(base_map = base_map_fun(), wms_layers = wms_layers)
    })
  })
}

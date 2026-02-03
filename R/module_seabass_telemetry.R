mod_seabass_ui <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      "Migration predictions",
      leafletOutput(ns("migration_map"), height = 700)
    ),
    tabPanel(
      "acoustic telemetry data",
      leafletOutput(ns("data_map"), height = 700),
      DTOutput(ns("acoustic_telemetry"))
    ),
    tabPanel(
      "Environmental layers",
      leafletOutput(ns("env_map"), height = 700)
    )
  )
}

mod_seabass_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$migration_map <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$Esri.OceanBasemap) |>
        setView(lng = 2.5, lat = 51.2, zoom = 6) |>
        addMarkers(lng = 2.5, lat = 51.2, popup = "Example point")
    })
    
    output$data_map <- renderLeaflet({
      get_acoustic_map()
    })
    
    output$env_map <- renderLeaflet({
      get_env_map()
    })
    
    output$acoustic_telemetry <- renderDT({
      datasets::cars
    })
  })
}

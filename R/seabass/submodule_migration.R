mod_seabass_migration_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons(
        ns("seabass_prediction"),
        "Seabass prediction layer",
        choices = c(
          "Inside OWF" = "inside",
          "Outside OWF" = "outside",
          "Difference inside/outside OWF" = "Diff OWF"
        )
      ),
      fluidRow(
        column(6, actionButton(ns("prev_month"), "◀ Previous", width = "100%")),
        column(6, actionButton(ns("next_month"), "Next ▶", width = "100%"))
      ),
      br(),
      sliderInput(ns("month"), "Month", min = 1, max = 12, value = 1, ticks = FALSE),
      tags$div(style = "text-align:center; font-weight:bold;", textOutput(ns("month_label")))
    ),
    mainPanel(
      leafletOutput(ns("seabass_migration_map"), height = 700)
    )
  )
}

mod_seabass_migration_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$month_label <- renderText({
      req(input$month)
      month.name[input$month]
    })
    
    current_raster_stack <- reactive({
      req(input$seabass_prediction)
      if (input$seabass_prediction == "inside") {
        prediction_layers[["Predictions inside OWF"]]
      } else if (input$seabass_prediction == "outside") {
        prediction_layers[["Predictions outside OWF"]]
      } else {
        prediction_layers[["Diff OWF"]]
      }
    })
    
    current_palette <- reactive({
      req(input$seabass_prediction)
      if (input$seabass_prediction == "inside") {
        prediction_palettes[["Predictions inside OWF"]]
      } else if (input$seabass_prediction == "outside") {
        prediction_palettes[["Predictions outside OWF"]]
      } else {
        prediction_palettes[["Diff OWF"]]
      }
    })
    
    output$seabass_migration_map <- renderLeaflet({
      req(input$month)
      r <- current_raster_stack()[[input$month]]
      pal <- current_palette()
      
      make_base_map() %>%
        leaflet::setView(lat = 51.5, lng = 2.5, zoom = 8) %>%
        leaflet::addRasterImage(r, colors = pal, opacity = 0.8, layerId = "raster") %>%
        leaflet::addLegend(pal = pal, values = raster::values(r), title = "Raster value")
    })
    
    observeEvent(input$prev_month, {
      updateSliderInput(session, "month", value = max(1, input$month - 1))
    })
    
    observeEvent(input$next_month, {
      updateSliderInput(session, "month", value = min(12, input$month + 1))
    })
    
    observe({
      req(input$month)
      r <- current_raster_stack()[[input$month]]
      pal <- current_palette()
      
      leafletProxy("seabass_migration_map", session = session) %>%
        clearImages() %>%
        clearControls() %>%
        addRasterImage(r, colors = pal, opacity = 0.8, layerId = "raster") %>%
        addLegend(pal = pal, values = raster::values(r), title = "Raster value")
    })
  })
}

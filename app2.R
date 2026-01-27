
library(shiny)
library(leaflet)
library(terra)
library(httr)
# library(leafem)

# -----------------------
# Create example rasters
# (replace with rast("seabass.nc"), rast("porpoise.nc"))
# -----------------------
make_raster <- function(seed) {
  set.seed(seed)
  r <- rast(
    nrows = 100, ncols = 100,
    xmin = 0, xmax = 5,
    ymin = 50, ymax = 52,
    nlyrs = 12
  )
  values(r) <- runif(ncell(r) * 12)
  names(r) <- month.name
  r
}

r_seabass  <- make_raster(1)
# Read into SpatRaster
r_porpoise <- make_raster(2)

# -----------------------
# Palettes (once, per species)
# -----------------------
pal_seabass <- colorNumeric(
  "viridis",
  domain = values(r_seabass),
  na.color = "transparent"
)

pal_porpoise <- colorNumeric(
  "magma",
  domain = values(r_porpoise),
  na.color = "transparent"
)

# -----------------------
# List of layers
# -----------------------
prediction_layers_info <- list(
  "Predictions inside OWF"        = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/predictions_inside_owf_median_months.nc",monthly = TRUE),
  "Predictions outside OWF"       = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/predictions_outside_owf_median_months.nc", monthly = TRUE),
  "Diff OWF"            = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/diff_owf.nc", monthly = TRUE)
)

env_layers_info <- list(
  "Bathymetry"          = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/bathy.nc", monthly = FALSE),
  "Habitats"            = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/habitats.nc", monthly = FALSE),
  "LOD median months"   = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/lod_median_months.nc", monthly = TRUE),
  "OWF distance"        = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/owf_dist.nc", monthly = FALSE),
  "Shipwreck distance"  = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/shipwreck_dist.nc", monthly = FALSE),
  "SST median months"   = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/sst_median_months.nc", monthly = TRUE),
  "X coords"            = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/x_m_4326.nc", monthly = FALSE),
  "Y coords"            = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/y_m_4326.nc", monthly = FALSE)
)

data_layers_info <- list(
  "Counts median months"        = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/counts_median_months.nc",monthly = TRUE),
  "N active tags median months" = list(url = "https://minio.dive.edito.eu/oidc-lottepohl/n_active_tags_median_months.nc", monthly = TRUE)
)

# -----------------------
# Load layers at startup
# -----------------------
load_layer <- function(url) {
  tf <- tempfile(fileext = ".nc")
  GET(url, write_disk(tf, overwrite = TRUE))
  terra::rast(tf)
}

prediction_layers <- lapply(prediction_layers_info, function(x) load_layer(x$url))
env_layers <- lapply(env_layers_info, function(x) load_layer(x$url))
data_layers <- lapply(data_layers_info, function(x) load_layer(x$url))

# -----------------------
# Create palettes for layers
# -----------------------
prediction_palettes <- lapply(prediction_layers, function(r) {
  colorNumeric("viridis", values(r), na.color = "transparent")
})

env_palettes <- lapply(env_layers, function(r) {
  colorNumeric("viridis", values(r), na.color = "transparent")
})

data_palettes <- lapply(data_layers, function(r) {
  colorNumeric("viridis", values(r), na.color = "transparent")
})

# -----------------------
# UI
# -----------------------
ui <- fluidPage(

  titlePanel("DUC4.2: Impact from offshore infrastructures on marine life"),

  tabsetPanel(

    # ---- Maps tab ----
    tabPanel(
      title = "Maps",

      sidebarLayout(
        sidebarPanel(
          width = 3,

          radioButtons(
            "species",
            "Species",
            choices = c(
              "Seabass" = "seabass",
              "Harbour porpoise" = "porpoise"
            )
          ),

          # Only show for seabass
          conditionalPanel(
            condition = "input.species == 'seabass'",
            radioButtons(
              "seabass_prediction",
              "Seabass prediction layer",
              choices = c(
                "Inside OWF" = "inside",
                "Outside OWF" = "outside",
                "Difference inside/outside OWF" = "Diff OWF"
              )
            )
          ),

          fluidRow(
            column(
              6,
              actionButton("prev_month", "◀ Previous", width = "100%")
            ),
            column(
              6,
              actionButton("next_month", "Next ▶", width = "100%")
            )
          ),

          br(),

          sliderInput(
            "month",
            "Month",
            min = 1,
            max = 12,
            value = 1,
            ticks = FALSE
          ),

          tags$div(
            style = "text-align:center; font-weight:bold;",
            textOutput("month_label")
          )
        ),

        mainPanel(
          leafletOutput("map", height = 600)
        )
      )
    ),

    # ---- Data tab ----
    tabPanel(
      title = "Data",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("data_layer", "Layer", choices = names(data_layers)),
          sliderInput("data_month", "Month (if applicable)", min = 1, max = 12, value = 1, ticks = FALSE)
        ),
        mainPanel(leafletOutput("data_map", height = 600))
      )
    ),

    # ---- Environmental Data tab ----
    tabPanel(
      title = "Environmental Data",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("env_layer", "Layer", choices = names(env_layers)),
          sliderInput("env_month", "Month (if applicable)", min = 1, max = 12, value = 1, ticks = FALSE)
        ),
        mainPanel(leafletOutput("env_map", height = 600))
      )
    ),

    # ---- About tab ----
    tabPanel(
      title = "About",
      fluidRow(
        column(
          width = 12,
          h3("About this app"),
          p("insert ABOUT text here")
        )
      )
    )
  )
)

# -----------------------
# Server
# -----------------------
server <- function(input, output, session) {

  # Month label
  output$month_label <- renderText({
    month.name[input$month]
  })

  # Reactive raster stack by species
  current_raster_stack <- reactive({
    if (input$species == "seabass") {
      # Use predictions for seabass
      if (input$seabass_prediction == "inside") {
        prediction_layers[["Predictions inside OWF"]]
      } else if(input$seabass_prediction == "outside") {
        prediction_layers[["Predictions outside OWF"]]
      } else {prediction_layers[["Diff OWF"]]}
    } else {
      r_porpoise
    }
  })

  # Reactive palette by species
  current_palette <- reactive({
    if (input$species == "seabass") {
      if (input$seabass_prediction == "inside") {
        prediction_palettes[["Predictions inside OWF"]]
      } else if (input$seabass_prediction == "outside") {
        prediction_palettes[["Predictions outside OWF"]]
      } else {
        prediction_palettes[["Diff OWF"]]
      }
    } else {
      pal_porpoise
    }
  })
  

  # Initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat = 51.5,
              lng = 2.5,
              zoom = 8) %>%
      # addHomeButton() %>%
      # leafem::addMouseCoordinates() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addRasterImage(
        r_seabass[[1]],
        colors = pal_seabass,
        opacity = 0.8,
        layerId = "raster"
      ) %>%
      addLegend(
        pal = pal_seabass,
        values = values(r_seabass),
        title = "Raster value"
      )
  })

  # Previous month button
  observeEvent(input$prev_month, {
    updateSliderInput(
      session,
      "month",
      value = max(1, input$month - 1)
    )
  })

  # Next month button
  observeEvent(input$next_month, {
    updateSliderInput(
      session,
      "month",
      value = min(12, input$month + 1)
    )
  })

  # Update raster + legend when species or month changes
  observe({
    req(input$month, input$species)

    leafletProxy("map") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(
        current_raster_stack()[[input$month]],
        colors = current_palette(),
        opacity = 0.8,
        layerId = "raster"
      ) %>%
      addLegend(
        pal = current_palette(),
        values = values(current_raster_stack()[[input$month]]),
        title = paste("Value –", input$species)
      )
  })

  # -----------------------
  # Data tab rendering
  # -----------------------
  output$data_map <- renderLeaflet({
    leaflet() %>%       
      setView(lat = 51.5,               
              lng = 2.5,               
              zoom = 8) %>% 
      addProviderTiles(providers$CartoDB.Positron)
  })

  observe({
    req(input$data_layer)
    r <- data_layers[[input$data_layer]]
    pal <- data_palettes[[input$data_layer]]

    # If monthly, subset by month
    if (data_layers_info[[input$data_layer]]$monthly) {
      r <- r[[input$data_month]]
    }

    leafletProxy("data_map") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(r, colors = pal, opacity = 0.8, layerId = "raster") %>%
      addLegend(pal = pal, values = values(r), title = input$data_layer)
  })

  # -----------------------
  # Environmental Data tab
  # -----------------------
  output$env_map <- renderLeaflet({
    leaflet() %>%      
      setView(lat = 51.5,               
              lng = 2.5,               
              zoom = 8) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })

  observe({
    req(input$env_layer)
    r <- env_layers[[input$env_layer]]
    pal <- env_palettes[[input$env_layer]]
    # If the layer has multiple months
    if (env_layers_info[[input$env_layer]]$monthly) {
      r <- r[[input$env_month]]
    }

    leafletProxy("env_map") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(r, colors = pal, opacity = 0.8, layerId = "raster") %>%
      addLegend(pal = pal, values = values(r), title = input$env_layer)
  })
}

# -----------------------
# Run app
# -----------------------
shinyApp(ui, server)

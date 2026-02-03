library(shiny)
library(DT)
library(leaflet)
#library(sass)
#library(yaml)
#library(memoise)
library(glue)

blue_light <- "#cde3f6" #"#b3d9f7"
blue_medium <- "#477292"
blue_dark <- "#11395a"


# load map with all environmental WMS layers ------------------------------

#maps
source("./maps/01_map_base.R")
source("./maps/02_map_environmental_layers.R")
source("./maps/03_map_acoustic_detections.R")

#helpers
source("./helpers/load_acoustic_telemetry_GAM_s3.R")

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(glue::glue("
    /* Make the header area (tabs + logo) a flex row */
    .top-tabs {{
      position: relative;
    }}

    /* The UL that holds the tabs */
    .top-tabs > .tabbable > .nav-tabs {{
      background-color: {blue_light} !important;
      padding: 6px 6px 0 6px;
      border-radius: 6px;

      display: flex;
      align-items: center;
      padding-right: 70px; /* space so the logo doesn't overlap tabs */
    }}

    /* The logo link positioned at the right edge of the tab strip */
    .top-tabs .top-tabs-logo {{
      position: absolute;
      right: 25px;
      top: 10px;
      z-index: 10;
      display: inline-flex;
      align-items: center;
      text-decoration: none;
    }}

    .top-tabs .top-tabs-logo img {{
      display: block;
    }}
    
    /* Style for the tab content (panels) and tabs in the lower-level sections */
    .lower-level-tabs > .tabbable > .nav-tabs {{
      background-color: {blue_dark} !important;
      color: white !important;
      padding: 6px 6px 0 6px;
      border-radius: 6px;
    }}
      
     /* Set the text color of the tab titles in the lower-level tabs */
      .lower-level-tabs .tabbable > .nav-tabs > li > a {{
      color: white !important;}}
      
      /* Set the text color of the active tab to blue_dark */
      .lower-level-tabs .tabbable > .nav-tabs > li.active > a {{
        color: {blue_dark} !important;}}
  ")))
  ),
  
  titlePanel("DUC2: Impact from offshore infrastructures on marine life"),
  mainPanel(width = 12,
            tags$div(                 
              class = "top-tabs container-fluid",
              
              # RIGHT-SIDE LOGO (clickable)
              tags$a(
                href = "https://dto-bioflow.eu/",
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
                
                tabPanel(
                  width = 12,
                  "Home", style = "font-size: 16px;",
                  fluidRow(
                    column(
                      width = 12,
                      h3("Welcome! 👋"),
                      p("Do you want to learn more about marine life and how it is influenced by offshore infrastructures? You're in the right place!"),
                      h4("How to use this app"),
                      p("<explanations>", style = "margin-bottom: 200px;"),
                      h4("Want to learn more?"),
                      p("Click the buttons below!"),
                      
                      tags$div(
                        tags$a(
                          "More info on DUC2",
                          href = "https://dto-bioflow.eu/use-cases/duc-2-impact-offshore-infrastructures",
                          target = "_blank",
                          rel = "noopener",
                          class = "btn btn-primary d-flex align-items-center justify-content-center",
                          style = glue::glue("margin-bottom:25px;background-color:{blue_medium}; color:white;")
                        ),
                        HTML("<br>"),
                        tags$a(
                          tagList(
                            tags$img(
                              src = "Logo_BIO-Flow2023_Final_Negative.png",
                              height = "50px",
                              style = "vertical-align:middle"
                            )
                          ),
                          href = "https://dto-bioflow.eu/",
                          target = "_blank",
                          rel = "noopener",
                          class = "btn btn-primary",
                          style = glue::glue("height:60px;background-color:{blue_medium}; color:white;")
                        )
                      )
                    )
                  )
                ),
                
                tabPanel(
                  width = 12,
                  title = tagList(
                    tags$img(
                      src = "D_labrax_phylopic_CC0.png",
                      height = "24px",
                      style = "vertical-align:middle; margin-right:8px;"
                    ),
                    tags$span("European seabass", style = "font-size: 16px; vertical-align:middle;")
                  ),
                  class = "lower-level-tabs",
                  tabsetPanel(
                    tabPanel("Migration predictions", 
                             
                             sidebarLayout(
                               sidebarPanel(
                                 width = 3,
                                 
                                 radioButtons(
                                   "seabass_prediction",
                                   "Seabass prediction layer",
                                   choices = c(
                                     "Inside OWF" = "inside",
                                     "Outside OWF" = "outside",
                                     "Difference inside/outside OWF" = "Diff OWF"
                                   )
                                 ),
                                 # ),
                                 
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
                                 leafletOutput("seabass_migration_map", height = 700)
                               )
                             )
                    ),
                    tabPanel("acoustic telemetry data",
                             leafletOutput("seabass_data_map", height = 700), 
                             DTOutput("acoustic_telemetry")),
                    tabPanel("Environmental layers", 
                             leafletOutput("env_data_map", height = 700))
                  )
                ),
                
                tabPanel(width = 12,
                         title = tagList(
                           tags$img(
                             src = "P_phocoena_phylopic_CC0.png",
                             height = "24px",
                             style = "vertical-align:middle; margin-right:8px;"
                           ),
                           tags$span("Harbour porpoise", style = "font-size: 16px; vertical-align:middle;")
                         ),
                         class = "lower-level-tabs",
                         tabsetPanel(
                           tabPanel("PAM dashboard", DTOutput("PAM_dashboard")),
                           tabPanel("PAM data", DTOutput("PAM_data")),
                           tabPanel("Habitat suitability (Marco-Bolo)", DTOutput("HSM_porpoise"))
                         )
                )
              )
            )
  )
)

server <- function(input, output, session) {
  
  # leaflet maps
  ## model prediction map
  
  # Month label
  output$month_label <- renderText({
    month.name[input$month]
  })
  
  # Reactive raster stack by species
  current_raster_stack <- reactive({
    if (input$seabass_prediction == "inside") {
      prediction_layers[["Predictions inside OWF"]]
    } else if(input$seabass_prediction == "outside") {
      prediction_layers[["Predictions outside OWF"]]
    } else {prediction_layers[["Diff OWF"]]}
  })
  
  # Reactive palette by species
  current_palette <- reactive({
    if (input$seabass_prediction == "inside") {
      prediction_palettes[["Predictions inside OWF"]]
    } else if (input$seabass_prediction == "outside") {
      prediction_palettes[["Predictions outside OWF"]]
    } else {
      prediction_palettes[["Diff OWF"]]
    }
  })
  
  
  # Initial map
  output$seabass_migration_map <- renderLeaflet({
    map_base %>%
      setView(lat = 51.5,
              lng = 2.5,
              zoom = 8) %>%
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
    req(input$month)
    
    leafletProxy("seabass_migration_map") %>%
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
        values = values(current_raster_stack()[[input$month]])
      )
  })
  
  ## raw acoustic telemetry data map
  output$seabass_data_map <- renderLeaflet({
    map_acoustic_detections
  })
  
  ## environmental layers map (with layers (mostly) from EDITO STAC catalogue)
  output$env_data_map <- renderLeaflet({
    map_WMS_EDITO_legend
  })
  
  output$PAM_dashboard <- renderDT(cars)
  output$PAM_data <- renderDT(cars)
  output$HSM_porpoise <- renderDT(head(cars, 3))
}

shinyApp(ui, server)
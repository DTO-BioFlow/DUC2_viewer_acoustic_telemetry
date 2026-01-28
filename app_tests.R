
# app.R
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(leaflet)
library(leaflet.minicharts)

set.seed(1)

# ---- Synthetic "your-shaped" data ----
stations <- tibble(
  station_name = paste0("S", 1:8),
  deploy_latitude  = runif(8, 51.0, 51.8),
  deploy_longitude = runif(8,  2.0,  3.6)
)

tag_ids <- paste0("T", 1:5)
months <- seq(ymd("2021-01-01"), ymd("2021-12-01"), by = "1 month")

# long format similar to your etn_monthyear_individual_sum
etn_long <- tidyr::crossing(
  monthyear = months,
  station_name = stations$station_name,
  tag_serial_number = tag_ids
) %>%
  left_join(stations, by = "station_name") %>%
  mutate(n_detections = rpois(n(), lambda = sample(0:40, 1))) %>%
  group_by(monthyear) %>%
  mutate(n_detections_monthyear = sum(n_detections)) %>%
  ungroup() %>%
  group_by(monthyear, station_name) %>%
  mutate(n_detections_monthyear_station = sum(n_detections)) %>%
  ungroup()

# wide per station-month (one row per station per month)
etn_wide <- etn_long %>%
  select(monthyear, station_name, deploy_longitude, deploy_latitude,
         tag_serial_number, n_detections,
         n_detections_monthyear, n_detections_monthyear_station) %>%
  pivot_wider(
    names_from = tag_serial_number,
    values_from = n_detections,
    names_prefix = "id_",
    values_fill = 0
  ) %>%
  arrange(station_name, monthyear)

# base stations for layerId vector
station_ids <- stations$station_name

ui <- fluidPage(
  titlePanel("Leaflet minicharts + built-in animation + prev/next (MWE)"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6, actionButton("prev_month", "◀ Previous", width = "100%")),
        column(6, actionButton("next_month", "Next ▶", width = "100%"))
      ),
      br(),
      checkboxInput("show_labels", "Show values in charts", FALSE),
      tags$div(style = "margin-top: 8px; font-weight: bold;", textOutput("month_label")),
      tags$div(
        style = "font-size: 12px; color: #555;",
        "Tip: the map also contains a built-in time slider (animation control)."
      )
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

server <- function(input, output, session) {
  
  # external month state (for prev/next buttons)
  idx <- reactiveVal(1)
  
  observeEvent(input$prev_month, {
    idx(max(1, idx() - 1))
  })
  
  observeEvent(input$next_month, {
    idx(min(length(months), idx() + 1))
  })
  
  selected_month <- reactive(months[[idx()]])
  
  output$month_label <- renderText({
    paste("Selected month:", format(selected_month(), "%B %Y"))
  })
  
  # initialize map with minicharts ONCE
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = mean(stations$deploy_longitude), lat = mean(stations$deploy_latitude), zoom = 8) %>%
      addMinicharts(
        lng = etn_wide$deploy_longitude,
        lat = etn_wide$deploy_latitude,
        layerId = etn_wide$station_name,
        # chartdata will be set via updateMinicharts()
        width = 45, height = 45,
        transitionTime = 750
      )
  })
  
  # update charts (this also enables animation because we pass time=)
  observe({
    chartdata <- etn_wide %>% select(starts_with("id_"))
    maxValue <- max(as.matrix(chartdata))
    
    leafletProxy("map") %>%
      updateMinicharts(
        layerId = station_ids,
        chartdata = chartdata,
        time = etn_wide$monthyear,          # <-- enables built-in time slider / animation
        timeFormat = "%b %Y",
        initialTime = selected_month(),     # <-- jump animation to the month chosen by prev/next
        type = "pie",
        maxValues = maxValue,
        showLabels = input$show_labels,
        transitionTime = 500,
        legend = TRUE,
        legendPosition = "topright"
      ) %>%
      clearControls() %>%
      addControl(
        html = sprintf(
          "<div style='font-size:14px;color:black;'>
             Detections on %s
           </div>",
          format(selected_month(), "%B %Y")
        ),
        position = "bottomleft"
      )
  })
}

shinyApp(ui, server)


# # app.R
# library(shiny)
# library(dplyr)
# library(tidyr)
# library(lubridate)
# library(leaflet)
# library(leaflet.minicharts)
# 
# set.seed(1)
# 
# # ---- Synthetic data in the shape of your table ----
# stations <- tibble(
#   station_name = paste0("S", 1:8),
#   deploy_latitude  = runif(8, 51.0, 51.8),
#   deploy_longitude = runif(8,  2.0,  3.6)
# )
# 
# tag_ids <- paste0("T", 1:5)   # <- rename (DON'T call this 'tags')
# months <- seq(ymd("2021-01-01"), ymd("2021-12-01"), by = "1 month")
# 
# etn <- tidyr::crossing(
#   monthyear = months,
#   station_name = stations$station_name,
#   tag_serial_number = tag_ids
# ) %>%
#   left_join(stations, by = "station_name") %>%
#   mutate(
#     n_detections = rpois(n(), lambda = sample(0:40, 1))
#   ) %>%
#   group_by(monthyear) %>%
#   mutate(n_detections_monthyear = sum(n_detections)) %>%
#   ungroup() %>%
#   group_by(monthyear, station_name) %>%
#   mutate(n_detections_monthyear_station = sum(n_detections)) %>%
#   ungroup()
# 
# deployments_minichart <- stations %>%
#   group_by(station_name) %>%
#   summarise(
#     lat = mean(deploy_latitude, na.rm = TRUE),
#     lon = mean(deploy_longitude, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# ui <- fluidPage(
#   titlePanel("Leaflet minicharts + month navigation (MWE)"),
#   sidebarLayout(
#     sidebarPanel(
#       fluidRow(
#         column(6, actionButton("prev_month", "◀ Previous", width = "100%")),
#         column(6, actionButton("next_month", "Next ▶", width = "100%"))
#       ),
#       br(),
#       sliderInput("month_idx", "Month", min = 1, max = length(months), value = 1, ticks = FALSE),
#       shiny::tags$div(   # explicit namespace is extra-safe
#         style = "text-align:center; font-weight:bold;",
#         textOutput("month_label")
#       )
#     ),
#     mainPanel(
#       leafletOutput("map", height = 600)
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   current_idx <- reactiveVal(1)
#   
#   observeEvent(input$month_idx, {
#     current_idx(input$month_idx)
#   }, ignoreInit = TRUE)
#   
#   observeEvent(input$prev_month, {
#     current_idx(max(1, current_idx() - 1))
#   })
#   
#   observeEvent(input$next_month, {
#     current_idx(min(length(months), current_idx() + 1))
#   })
#   
#   observeEvent(current_idx(), {
#     updateSliderInput(session, "month_idx", value = current_idx())
#   })
#   
#   selected_month <- reactive({
#     months[[current_idx()]]
#   })
#   
#   output$month_label <- renderText({
#     format(selected_month(), "%B %Y")
#   })
#   
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(providers$CartoDB.Positron) %>%
#       setView(lng = mean(stations$deploy_longitude), lat = mean(stations$deploy_latitude), zoom = 8) %>%
#       addCircleMarkers(
#         data = deployments_minichart,
#         lng = ~lon, lat = ~lat,
#         radius = 4, stroke = FALSE, fillOpacity = 1, fillColor = "grey"
#       )
#   })
#   
#   month_minichart_df <- reactive({
#     m <- selected_month()
#     
#     long <- etn %>%
#       filter(monthyear == m) %>%
#       select(monthyear, station_name, tag_serial_number,
#              deploy_latitude, deploy_longitude,
#              n_detections, n_detections_monthyear, n_detections_monthyear_station)
#     
#     long %>%
#       pivot_wider(
#         names_from = tag_serial_number,
#         values_from = n_detections,
#         names_prefix = "id_",
#         values_fill = 0
#       )
#   })
#   
#   observe({
#     df <- month_minichart_df()
#     
#     denom <- df$n_detections_monthyear
#     denom[denom == 0] <- 1
#     
#     widths <- 60 * sqrt(df$n_detections_monthyear_station) / sqrt(denom)
#     widths[!is.finite(widths)] <- 0
#     
#     leafletProxy("map") %>%
#       clearMarkers() %>%      # clears the circle markers AND minicharts (they are markers internally)
#       clearControls() %>%
#       addCircleMarkers(       # redraw station dots (optional if you want them under pies)
#         data = deployments_minichart,
#         lng = ~lon, lat = ~lat,
#         radius = 4, stroke = FALSE, fillOpacity = 1, fillColor = "grey"
#       ) %>%
#       addMinicharts(
#         lng = df$deploy_longitude,
#         lat = df$deploy_latitude,
#         type = "pie",
#         chartdata = df %>% select(starts_with("id_")),
#         width = widths,
#         opacity = 0.9,
#         transitionTime = 0
#       ) %>%
#       addControl(
#         html = sprintf(
#           "<div style='font-size:14px;color:black;'>
#            Detections on %s
#          </div>",
#           format(selected_month(), "%B %Y")
#         ),
#         position = "bottomleft"
#       )
#   })
#   
# }
# 
# shinyApp(ui, server)

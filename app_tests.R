# Fully copy/paste MWE -----------------------------------------------

library(shiny)
library(leaflet)
library(leaflet.minicharts)
library(dplyr)
library(tidyr)
library(htmlwidgets)

# load the base map
map_base <- readRDS("./maps/01_map_base.rds")
deployments <- readRDS("./data/deployments.rds")
etn_monthyear_individual_sum <- readRDS("./data/etn_sum_seabass_monthyear_indivdual.rds")

deployments_minichart <-
  deployments %>%
  dplyr::group_by(station_name) %>%
  dplyr::summarise(lat = mean(deploy_latitude, na.rm = T),
                   lon = mean(deploy_longitude, na.rm = T)) %>%
  dplyr::ungroup()

# 1) One row per station (coordinates only)
stations <- deployments_minichart %>%
  distinct(station_name, lon, lat) %>%
  arrange(station_name)

detections_wide <-
  etn_monthyear_individual_sum %>%
  dplyr::mutate(month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01"))) %>%
  # dplyr::filter(monthyear %>% date() == selected_date) %>%
  tidyr::pivot_wider(names_from = tag_serial_number, values_from = n_detections, names_prefix = "id_",
                     values_fill  = 0) %>%
  dplyr::ungroup()

ids <- names(detections_wide %>% select(starts_with("id_")))
months <- detections_wide$month %>% unique() %>% sort()

# IMPORTANT: full grid (station x month), in a strict order:
# month blocks, each containing stations in station order
anim_df <- expand_grid(
  month = months,
  station_name = stations$station_name
) %>%
  left_join(detections_wide, by = c("month", "station_name")) %>%
  mutate(across(all_of(ids), ~ replace_na(.x, 0))) %>%
  arrange(month, station_name) %>%
  left_join(stations, by = "station_name")

chartdata_all <- as.matrix(anim_df[, ids])      # nrow = nStations * nMonths
time_all <- anim_df$month                       # same length as nrow(chartdata_all)

# --- Shiny app ------------------------------------------------------

ui <- fluidPage(
  titlePanel("Leaflet minicharts time animation (MWE)"),
  sidebarLayout(
    sidebarPanel(height = 2000,
      selectInput("prods", "Select individuals", choices = ids, selected = ids, multiple = TRUE),
      column(6, actionButton("prev_month", "◀ Previous month", width = "100%")),
      column(6, actionButton("next_month", "Next month ▶", width = "100%")),
      selectInput("type", "Chart type", choices = c("pie", "bar", "polar-area", "polar-radius")),
      checkboxInput("labels", "Show values", value = FALSE),
      uiOutput("month_summary")
    ),
    mainPanel(
      leafletOutput("map", height = 1000)
    )
  )
)

server <- function(input, output, session) {
  
  month_idx <- reactiveVal(1)
  
  output$month_summary <- renderUI({
    
    i <- month_idx()
    
    # make it scalar + valid, without ever evaluating vector logicals
    if (length(i) != 1L) i <- 1L
    i <- as.integer(i[1])
    # if (is.na(i) || i < 1L || i > length(months)) i <- 1L
    i <- month_idx()
    if (length(i) != 1L) i <- 1L
    i <- as.integer(i[1])
    if (is.na(i)) i <- 1L
    if (i < 1L) i <- 1L
    if (i > length(months)) i <- length(months)
    
    m <- months[i]
    
    df_m <- etn_monthyear_individual_sum %>%
      mutate(month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01"))) %>%
      filter(month == m)
    
    if (nrow(df_m) == 0) {
      return(tagList(
        tags$hr(),
        tags$b(format(m, "%Y-%m")),
        tags$div("No detections for this month.")
      ))
    }
    
    n_indiv <- df_m %>%
      filter(n_detections > 0) %>%
      summarise(n = n_distinct(tag_serial_number), .groups = "drop") %>%
      pull(n)
    # n_indiv <- ifelse(length(n_indiv) == 0 || is.na(n_indiv), 0, n_indiv)
    n_indiv <- df_m %>%
      filter(n_detections > 0) %>%
      summarise(n = n_distinct(tag_serial_number), .groups = "drop") %>%
      pull(n)
    
    if (length(n_indiv) == 0) n_indiv <- 0
    n_indiv <- as.integer(n_indiv[1])
    if (is.na(n_indiv)) n_indiv <- 0
    
    
        
    total_det <- sum(df_m$n_detections, na.rm = TRUE)
    
    top_station_indiv <- df_m %>%
      filter(n_detections > 0) %>%
      group_by(station_name) %>%
      summarise(n_indiv = n_distinct(tag_serial_number), .groups = "drop") %>%
      arrange(desc(n_indiv)) %>%
      slice_head(n = 1)
    
    top_indiv_name <- if (nrow(top_station_indiv) == 0) "—" else top_station_indiv$station_name[1]
    top_indiv_val  <- if (nrow(top_station_indiv) == 0) 0   else top_station_indiv$n_indiv[1]
    
    top_station_det <- df_m %>%
      group_by(station_name) %>%
      summarise(n_det = sum(n_detections, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(n_det)) %>%
      slice_head(n = 1)
    
    top_det_name <- if (nrow(top_station_det) == 0) "—" else top_station_det$station_name[1]
    top_det_val  <- if (nrow(top_station_det) == 0) 0   else top_station_det$n_det[1]
    
    tagList(
      tags$hr(),
      tags$div(
        tags$b(format(m, "%Y-%m")),
        tags$ul(
          tags$li(paste0("Number of individuals detected: ", n_indiv)),
          tags$li(paste0("Total detections this month: ", total_det)),
          tags$li(paste0("Station with most individuals: ", top_indiv_name, " (", top_indiv_val, ")")),
          tags$li(paste0("Station with most detections: ", top_det_name, " (", top_det_val, ")"))
        )
      )
    )
  })
  
  
  
  
  output$map <- renderLeaflet({
    init_prods <- ids
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(stations$lon), lat = mean(stations$lat), zoom = 11) %>%
      addCircleMarkers(
        data = stations,
        lng = ~lon, lat = ~lat,
        radius = 4, stroke = FALSE, fillOpacity = 1, fillColor = "grey"
      ) %>%
      addMinicharts(
        lng = stations$lon,
        lat = stations$lat,
        layerId = stations$station_name,
        # type = "pie",
        
        # Start with all ids selected
        chartdata = anim_df[, init_prods, drop = FALSE],
        legendPosition = "topleft",
        # legend = F,
        
        # This enables the time slider/play control
        time = anim_df$month,
        # timeFormat = "%Y-%m",
        # initialTime = min(time_all),
        # # This enables the time slider/play control
        # time = time_all,
        # timeFormat = "%Y-%m",
        # initialTime = min(time_all),
        
        width = 45, #60 * sqrt(anim_df$n_detections_monthyear_station) / sqrt(anim_df$n_detections_monthyear),
        height = 45
      ) %>%
      onRender("
      function(el, x) {
        function styleLegend() {
          // find the leaflet control that contains id_ labels
          var ctrls = el.querySelectorAll('.leaflet-control');
          var legend = null;
          for (var i = 0; i < ctrls.length; i++) {
            var txt = (ctrls[i].textContent || '');
            if (txt.match(/\\bid_\\w+/)) { legend = ctrls[i]; break; }
          }
          if (!legend) return false;

          legend.style.maxHeight = '300px';
          legend.style.overflowY = 'auto';
          legend.style.overflowX = 'hidden';
          legend.style.maxWidth  = '500px';

          if (window.L && L.DomEvent) {
            L.DomEvent.disableClickPropagation(legend);
            L.DomEvent.disableScrollPropagation(legend);
          }
          return true;
        }

        // try now, else observe DOM changes until it appears
        if (!styleLegend()) {
          var obs = new MutationObserver(function() {
            if (styleLegend()) obs.disconnect();
          });
          obs.observe(el, { childList: true, subtree: true });
        }
      }
    ")
  })
  
  observeEvent(input$prev_month, {
    i <- month_idx()
    month_idx(if (i <= 1) length(months) else i - 1)
  })
  
  observeEvent(input$next_month, {
    i <- month_idx()
    month_idx(if (i >= length(months)) 1 else i + 1)
  })
  
  observeEvent(month_idx(), {
    leafletProxy("map", session) %>%
      updateMinicharts(
        layerId = stations$station_name,
        initialTime = months[month_idx()]   # <-- jump the time slider to this month
      )
  }, ignoreInit = TRUE)
  
  observe({
    prods <- input$prods
    
    data <- if (length(prods) == 0) {
      matrix(0, nrow = nrow(chartdata_all), ncol = 1)
    } else {
      as.matrix(anim_df[, prods, drop = FALSE])
    }
    maxValue <- max(as.matrix(data))
    
    leafletProxy("map", session) %>%
      updateMinicharts(
        layerId = stations$station_name,
        chartdata = data,
        maxValues = maxValue,
        type = if (ncol(data) < 2) "polar-area" else input$type,
        showLabels = input$labels,
        time = anim_df$month
      )
  })
}

shinyApp(ui, server)


# demo with AT data -------------------------------------------------------

# library(dplyr)
# library(shiny)
# library(leaflet)
# library(leaflet.minicharts)
# 
# # User interface
# # data("regions")
# 
# # load the base map
# map_base <- readRDS("./maps/01_map_base.rds")
# deployments <- readRDS("./data/deployments.rds")
# etn_monthyear_individual_sum <- readRDS("./data/etn_sum_seabass_monthyear_indivdual.rds")
# 
# deployments_minichart <- 
#   deployments %>%
#   dplyr::group_by(station_name) %>%
#   dplyr::summarise(lat = mean(deploy_latitude, na.rm = T),
#                    lon = mean(deploy_longitude, na.rm = T)) %>%
#   dplyr::ungroup()
# 
# # 1) One row per station (coordinates only)
# stations <- deployments_minichart %>%
#   distinct(station_name, lon, lat) %>%
#   arrange(station_name)
# 
# etn_monthyear_individual_sum_minichart <-
#   etn_monthyear_individual_sum %>%
#   dplyr::mutate(month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01"))) %>%
#   # dplyr::filter(monthyear %>% date() == selected_date) %>%
#   tidyr::pivot_wider(names_from = tag_serial_number, values_from = n_detections, names_prefix = "id_",
#                      values_fill  = 0) %>%
#   dplyr::ungroup()
# 
# months <- etn_monthyear_individual_sum_minichart$month %>% unique() %>% sort()
# 
# # 3) Ensure every station exists for every month (required for clean animation)
# grid <- tidyr::expand_grid(station_name = stations$station_name, month = months)
# 
# 
# # Production columns
# ids <- names(etn_monthyear_individual_sum_minichart %>% select(starts_with("id_")))
# 
# # Create base map
# 
# basemap <- #leaflet(width = "100%", height = "400px") %>%
#   map_base %>%
#   addCircleMarkers(data = deployments_minichart,
#                    lat = ~lat,
#                    lng = ~lon,
#                    radius = 5,
#                    weight = 0,
#                    fillOpacity = 1,
#                    fillColor =  "grey")
#   # addTiles(tilesURL)# %>%
# # addPolylines(data = regions, color = "brown", weight = 1, fillOpacity = 0)
# 
# ui <- fluidPage(
#   titlePanel("Seabass acoustic detections"),
#   p(""),
#   
#   sidebarLayout(
#     
#     sidebarPanel(
#       selectInput(
#         "prods",
#         "Select individuals",
#         choices = ids,
#         selected = ids,   # <- select all by default
#         multiple = TRUE
#       ),
#       selectInput("type", "Chart type", choices = c("pie", "bar", "polar-area", "polar-radius")),
#       checkboxInput("labels", "Show values")
#     ),
#     
#     mainPanel(
#       leafletOutput("map", height = 900)
#     )
#     
#   )
# )
# 
# # server function
# server <- function(input, output, session) {
#   # Initialize map
#   output$map <- renderLeaflet({
#     basemap %>%
#       addMinicharts(
#         etn_monthyear_individual_sum_minichart$deploy_longitude, etn_monthyear_individual_sum_minichart$deploy_latitude,
#         layerId = etn_monthyear_individual_sum_minichart$station_name,
#         width = 60 * sqrt(etn_monthyear_individual_sum_minichart$n_detections_monthyear_station) / sqrt(etn_monthyear_individual_sum_minichart$n_detections_monthyear)
#         , height = 45
#       )
#   })
#   
#   # Update charts each time input value changes
#   observe({
#     if (length(input$prods) == 0) {
#       data <- 1
#     } else {
#       data <- etn_monthyear_individual_sum_minichart[, input$prods]
#     }
#     maxValue <- max(as.matrix(data))
#     
#     leafletProxy("map", session) %>%
#       updateMinicharts(
#         etn_monthyear_individual_sum_minichart$station_name,
#         chartdata = data,
#         maxValues = maxValue,
#         time = etn_monthyear_individual_sum_minichart$month,
#         type = ifelse(length(input$prods) < 2, "polar-area", input$type),
#         showLabels = input$labels
#       )
#   })
# }
# 
# shinyApp(ui, server)

# minicharts demo ---------------------------------------------------------

# library(dplyr)
# library(shiny)
# library(leaflet)
# library(leaflet.minicharts)
# 
# # User interface
# # data("regions")
# data("eco2mix")
# 
# # Remove data for the whole country
# prodRegions <- eco2mix %>% filter(area != "France")
# 
# # Production columns
# prodCols <- names(prodRegions)[6:13]
# 
# # Create base map
# tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
# 
# basemap <- leaflet(width = "100%", height = "400px") %>%
#   addTiles(tilesURL)# %>%
#   # addPolylines(data = regions, color = "brown", weight = 1, fillOpacity = 0)
# 
# ui <- fluidPage(
#   titlePanel("Demo of leaflet.minicharts"),
#   p("This application uses the data.frame 'eco2mix', included in the 'leaflet.minicharts' packages.",
#     "It contains the monthly electric production of french regions from 2013 to 2017."),
#   
#   sidebarLayout(
#     
#     sidebarPanel(
#       selectInput("prods", "Select productions", choices = prodCols, multiple = TRUE),
#       selectInput("type", "Chart type", choices = c("bar","pie", "polar-area", "polar-radius")),
#       checkboxInput("labels", "Show values")
#     ),
#     
#     mainPanel(
#       leafletOutput("map")
#     )
#     
#   )
# )
# 
# # server function
# server <- function(input, output, session) {
#   # Initialize map
#   output$map <- renderLeaflet({
#     basemap %>%
#       addMinicharts(
#         prodRegions$deploy_longitude, prodRegions$lat,
#         layerId = prodRegions$area,
#         width = 45, height = 45
#       )
#   })
#   
#   # Update charts each time input value changes
#   observe({
#     if (length(input$prods) == 0) {
#       data <- 1
#     } else {
#       data <- prodRegions[, input$prods]
#     }
#     maxValue <- max(as.matrix(data))
#     
#     leafletProxy("map", session) %>%
#       updateMinicharts(
#         prodRegions$area,
#         chartdata = data,
#         maxValues = maxValue,
#         time = prodRegions$month,
#         type = ifelse(length(input$prods) < 2, "polar-area", input$type),
#         showLabels = input$labels
#       )
#   })
# }
# 
# shinyApp(ui, server)


# xxx ---------------------------------------------------------------------



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
# stations <- tibble(
#   station_name = paste0("S", 1:8),
#   deploy_latitude  = runif(8, 51.0, 51.8),
#   deploy_longitude = runif(8,  2.0,  3.6)
# )
# 
# tag_ids <- paste0("T", 1:5)
# months <- seq(ymd("2021-01-01"), ymd("2021-12-01"), by = "1 month")
# 
# etn_long <- tidyr::crossing(
#   monthyear = months,
#   station_name = stations$station_name,
#   tag_serial_number = tag_ids
# ) %>%
#   left_join(stations, by = "station_name") %>%
#   mutate(n_detections = rpois(n(), lambda = sample(0:40, 1))) %>%
#   group_by(monthyear) %>%
#   mutate(n_detections_monthyear = sum(n_detections)) %>%
#   ungroup() %>%
#   group_by(monthyear, station_name) %>%
#   mutate(n_detections_monthyear_station = sum(n_detections)) %>%
#   ungroup()
# 
# etn_wide <- etn_long %>%
#   select(monthyear, station_name, deploy_longitude, deploy_latitude,
#          tag_serial_number, n_detections,
#          n_detections_monthyear, n_detections_monthyear_station) %>%
#   pivot_wider(
#     names_from = tag_serial_number,
#     values_from = n_detections,
#     names_prefix = "id_",
#     values_fill = 0
#   ) %>%
#   arrange(station_name, monthyear)
# 
# # --- chart matrix and max for consistent scaling across time
# chartdata_all <- etn_wide %>% select(starts_with("id_"))
# maxValue <- max(as.matrix(chartdata_all))
# 
# # --- size scaling (station share of month total), clamped to reasonable range
# den <- pmax(etn_wide$n_detections_monthyear, 1)
# sizes <- 60 * sqrt(etn_wide$n_detections_monthyear_station) / sqrt(den)
# sizes[!is.finite(sizes)] <- 0
# sizes <- pmin(pmax(sizes, 8), 80)  # min 8px, max 80px (tweak as you like)
# 
# ui <- fluidPage(
#   titlePanel("Leaflet minicharts + animation + prev/next (MWE)"),
#   sidebarLayout(
#     sidebarPanel(
#       fluidRow(
#         column(6, actionButton("prev_month", "◀ Previous", width = "100%")),
#         column(6, actionButton("next_month", "Next ▶", width = "100%"))
#       ),
#       br(),
#       checkboxInput("show_labels", "Show values in charts", FALSE),
#       tags$div(style = "margin-top: 8px; font-weight: bold;", textOutput("month_label"))
#     ),
#     mainPanel(
#       leafletOutput("map", height = 650)
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   idx <- reactiveVal(1)
#   
#   observeEvent(input$prev_month, {
#     idx(max(1, idx() - 1))
#   })
#   observeEvent(input$next_month, {
#     idx(min(length(months), idx() + 1))
#   })
#   
#   selected_month <- reactive(months[[idx()]])
#   
#   output$month_label <- renderText({
#     paste("Selected month:", format(selected_month(), "%B %Y"))
#   })
#   
#   # IMPORTANT: put time= (and chartdata) in addMinicharts so the time pane is created
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(providers$CartoDB.Positron) %>%
#       setView(deploy_longitude = mean(stations$deploy_longitude),
#               lat = mean(stations$deploy_latitude),
#               zoom = 8) %>%
#       addMinicharts(
#         deploy_longitude = etn_wide$deploy_longitude,
#         lat = etn_wide$deploy_latitude,
#         layerId = etn_wide$station_name,
#         
#         chartdata = chartdata_all,
#         type = "pie",
#         maxValues = maxValue,
#         
#         # animation/time control
#         time = etn_wide$monthyear,
#         timeFormat = "%b %Y",
#         initialTime = selected_month(),
#         
#         # per-row sizing (station-month)
#         width = sizes,
#         height = sizes,
#         
#         showLabels = input$show_labels,
#         transitionTime = 500,
#         
#         legend = TRUE,
#         legendPosition = "topright"
#       )
#   })
#   
#   # Update only what changes (DON'T clearControls, it removes the legend)
#   observeEvent(list(selected_month(), input$show_labels), {
#     leafletProxy("map") %>%
#       updateMinicharts(
#         layerId = unique(etn_wide$station_name),
#         
#         # keep same data/time
#         chartdata = chartdata_all,
#         time = etn_wide$monthyear,
#         timeFormat = "%b %Y",
#         
#         # jump to chosen month
#         initialTime = selected_month(),
#         
#         type = "pie",
#         maxValues = maxValue,
#         
#         # keep sizing
#         width = sizes,
#         height = sizes,
#         
#         showLabels = input$show_labels,
#         transitionTime = 250,
#         
#         legend = TRUE,
#         legendPosition = "topright"
#       )
#   }, ignoreInit = TRUE)
# }
# 
# shinyApp(ui, server)

# 
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
# # ---- Synthetic "your-shaped" data ----
# stations <- tibble(
#   station_name = paste0("S", 1:8),
#   deploy_latitude  = runif(8, 51.0, 51.8),
#   deploy_longitude = runif(8,  2.0,  3.6)
# )
# 
# tag_ids <- paste0("T", 1:5)
# months <- seq(ymd("2021-01-01"), ymd("2021-12-01"), by = "1 month")
# 
# # long format similar to your etn_monthyear_individual_sum
# etn_long <- tidyr::crossing(
#   monthyear = months,
#   station_name = stations$station_name,
#   tag_serial_number = tag_ids
# ) %>%
#   left_join(stations, by = "station_name") %>%
#   mutate(n_detections = rpois(n(), lambda = sample(0:40, 1))) %>%
#   group_by(monthyear) %>%
#   mutate(n_detections_monthyear = sum(n_detections)) %>%
#   ungroup() %>%
#   group_by(monthyear, station_name) %>%
#   mutate(n_detections_monthyear_station = sum(n_detections)) %>%
#   ungroup()
# 
# # wide per station-month (one row per station per month)
# etn_wide <- etn_long %>%
#   select(monthyear, station_name, deploy_longitude, deploy_latitude,
#          tag_serial_number, n_detections,
#          n_detections_monthyear, n_detections_monthyear_station) %>%
#   pivot_wider(
#     names_from = tag_serial_number,
#     values_from = n_detections,
#     names_prefix = "id_",
#     values_fill = 0
#   ) %>%
#   arrange(station_name, monthyear)
# 
# # base stations for layerId vector
# station_ids <- stations$station_name
# 
# ui <- fluidPage(
#   titlePanel("Leaflet minicharts + built-in animation + prev/next (MWE)"),
#   sidebarLayout(
#     sidebarPanel(
#       fluidRow(
#         column(6, actionButton("prev_month", "◀ Previous", width = "100%")),
#         column(6, actionButton("next_month", "Next ▶", width = "100%"))
#       ),
#       br(),
#       checkboxInput("show_labels", "Show values in charts", FALSE),
#       tags$div(style = "margin-top: 8px; font-weight: bold;", textOutput("month_label")),
#       tags$div(
#         style = "font-size: 12px; color: #555;",
#         "Tip: the map also contains a built-in time slider (animation control)."
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
#   # external month state (for prev/next buttons)
#   idx <- reactiveVal(1)
#   
#   observeEvent(input$prev_month, {
#     idx(max(1, idx() - 1))
#   })
#   
#   observeEvent(input$next_month, {
#     idx(min(length(months), idx() + 1))
#   })
#   
#   selected_month <- reactive(months[[idx()]])
#   
#   output$month_label <- renderText({
#     paste("Selected month:", format(selected_month(), "%B %Y"))
#   })
#   
#   # initialize map with minicharts ONCE
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(providers$CartoDB.Positron) %>%
#       setView(deploy_longitude = mean(stations$deploy_longitude), lat = mean(stations$deploy_latitude), zoom = 8) %>%
#       addMinicharts(
#         deploy_longitude = etn_wide$deploy_longitude,
#         lat = etn_wide$deploy_latitude,
#         layerId = etn_wide$station_name,
#         # chartdata will be set via updateMinicharts()
#         width = 45, height = 45,
#         transitionTime = 750
#       )
#   })
#   
#   # update charts (this also enables animation because we pass time=)
#   observe({
#     chartdata <- etn_wide %>% select(starts_with("id_"))
#     maxValue <- max(as.matrix(chartdata))
#     
#     leafletProxy("map") %>%
#       updateMinicharts(
#         layerId = station_ids,
#         chartdata = chartdata,
#         time = etn_wide$monthyear,          # <-- enables built-in time slider / animation
#         timeFormat = "%b %Y",
#         initialTime = selected_month(),     # <-- jump animation to the month chosen by prev/next
#         type = "pie",
#         maxValues = maxValue,
#         showLabels = input$show_labels,
#         transitionTime = 500,
#         legend = TRUE,
#         legendPosition = "topright"
#       ) %>%
#       clearControls() %>%
#       addControl(
#         html = sprintf(
#           "<div style='font-size:14px;color:black;'>
#              Detections on %s
#            </div>",
#           format(selected_month(), "%B %Y")
#         ),
#         position = "bottomleft"
#       )
#   })
# }

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
#       setView(deploy_longitude = mean(stations$deploy_longitude), lat = mean(stations$deploy_latitude), zoom = 8) %>%
#       addCircleMarkers(
#         data = deployments_minichart,
#         deploy_longitude = ~lon, lat = ~lat,
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
#         deploy_longitude = ~lon, lat = ~lat,
#         radius = 4, stroke = FALSE, fillOpacity = 1, fillColor = "grey"
#       ) %>%
#       addMinicharts(
#         deploy_longitude = df$deploy_longitude,
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


library(shiny)
library(leaflet)
library(leaflet.minicharts)
library(dplyr)
library(tidyr)
library(htmlwidgets)
library(RColorBrewer)

# bugs: once clicled on prev/next month, yp canno click the animation pane anymroe
# removing fish doesn;t work anymore
# TODO: current month bold top right, remove minimap? Legend about size of pies.
# only show in legend the individuals that were detected that month

# load the base map
map_base <- readRDS("maps_to_delete/01_map_base.rds")
deployments <- readRDS("data/deployments.rds")
etn_monthyear_individual_sum <- readRDS("data/etn_sum_seabass_monthyear_individual_subset.rds")

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
# time_all <- anim_df$month                       # same length as nrow(chartdata_all)


# custom width&height -----------------------------------------------------

# totals per station-month
station_month_totals <- etn_monthyear_individual_sum %>%
  mutate(month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01"))) %>%
  group_by(month, station_name) %>%
  summarise(n_station = sum(n_detections, na.rm = TRUE), .groups = "drop")

# totals per month (all stations)
month_totals <- station_month_totals %>%
  group_by(month) %>%
  summarise(n_month = sum(n_station, na.rm = TRUE), .groups = "drop")

# join onto anim_df (anim_df is month x station_name ordered!)
anim_df <- anim_df %>%
  left_join(station_month_totals, by = c("month", "station_name")) %>%
  left_join(month_totals, by = "month") %>%
  mutate(
    n_station = replace_na(n_station, 0),
    n_month   = pmax(replace_na(n_month, 0), 1),
    rel = n_station / n_month,
    
    # size formula (tune these)
    pie_size = 12 + 80 * sqrt(rel)
  )

width_all  <- anim_df$pie_size
height_all <- anim_df$pie_size

# custom legend -----------------------------------------

permute_spread <- function(n) {
  lo <- 1:ceiling(n/2)
  hi <- n:floor(n/2 + 1)
  as.vector(rbind(lo, hi))[1:n]
}

# Candidate pool (make it long)
base_pool <- c(
  brewer.pal(8, "Set2"),
  brewer.pal(12, "Paired"),
  brewer.pal(8,  "Dark2"),
  brewer.pal(9,  "Set1")#,
  # brewer.pal(12, "Set3")   # optional extra pool
)

# Helper: drop grey-ish colors (low chroma in HCL space)
drop_greys <- function(cols, min_chroma = 25) {
  rgb <- t(grDevices::col2rgb(cols)) / 255
  lab <- grDevices::convertColor(rgb, from = "sRGB", to = "Lab")
  a <- lab[, 2]
  b <- lab[, 3]
  chroma <- sqrt(a^2 + b^2)   # distance from grey axis
  cols[chroma >= min_chroma]
}

# Filter, then ensure we have enough
pool_filtered <- drop_greys(base_pool, min_chroma = 25)

# If filtering removed too many, relax the threshold a bit
if (length(pool_filtered) < length(ids)) {
  pool_filtered <- drop_greys(base_pool, min_chroma = 15)
}

# Repeat until long enough
base_cols <- rep(pool_filtered, length.out = length(ids))

# Spread neighbors
perm <- permute_spread(length(ids))
id_palette <- setNames(base_cols[perm], ids)


make_legend_html <- function(id_vec) {
  if (length(id_vec) == 0) {
    return("<div class='id-legend-scroll'><i>No individuals detected</i></div>")
  }
  items <- paste0(
    "<div style='display:flex;align-items:center;gap:6px;margin:2px 0;'>",
    "<span style='width:12px;height:12px;display:inline-block;background:", id_palette[id_vec], ";'></span>",
    "<span>", id_vec, "</span>",
    "</div>",
    collapse = ""
  )
  paste0("<div class='id-legend-scroll'>", items, "</div>")
}


# --- Shiny app ------------------------------------------------------


ui <- fluidPage(
  
  # legend CSS -> todo outsource  ------------
  
  tags$head(tags$style(HTML("
  .idLegendCtrl{
    background: white;
    padding: 10px 12px;
    border-radius: 6px;
    box-shadow: 0 1px 6px rgba(0,0,0,0.25);
  }

  .idLegendCtrl .id-legend-scroll{
    width: 150px;        /* <- make legend panel wider */
    max-height: 600px;
    overflow-y: auto;
    overflow-x: hidden;
  }

  .idLegendCtrl .id-legend-item{
    display: flex;
    align-items: center;
    gap: 8px;
    margin: 2px 0;
  }

  .idLegendCtrl .id-legend-swatch{
    width: 16px;
    height: 16px;
    display: inline-block;
    border-radius: 2px;
    border: 1px solid rgba(0,0,0,0.15);
    flex: 0 0 auto;
  }
"))),
  
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
  
  
  # helpers for current month/monthly summary -------------------------------
  
  month_idx <- reactiveVal(1)
  
  current_month <- reactive({
    months[month_idx()]
  })
  
  active_ids_this_month <- reactive({
    prods <- input$prods
    if (length(prods) == 0) return(character(0))
    
    m <- current_month()
    rows_m <- anim_df$month == m
    present <- colSums(anim_df[rows_m, prods, drop = FALSE], na.rm = TRUE) > 0
    prods[present]
  })
  
  make_popup_html <- function(vals_mat, station_vec, month_vec, id_names) {
    vapply(seq_len(nrow(vals_mat)), function(r) {
      vals <- vals_mat[r, ]
      nz <- which(vals > 0)
      header <- paste0("<b>", station_vec[r], "</b><br>", format(month_vec[r], "%Y-%m"), "<br>")
      if (length(nz) == 0) return(paste0(header, "<i>No detections</i>"))
      lines <- paste0(id_names[nz], ": ", vals[nz], collapse = "<br>")
      paste0(header, lines)
    }, character(1))
  }
  
  # summary for selected month ----------------------------------------------
  
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
          tags$li(paste0("Total detections this month: ", n_indiv)),
          tags$li(paste0("Station with most individuals: ", top_indiv_name, " (n = ", top_indiv_val, ")")),
          tags$li(paste0("Station with most detections: ", top_det_name, " (n = ", top_det_val, ")"))
        )
      )
    )
  })
  
  
  
  # leaflet map -------------------------------------------------------------
  
  output$map <- renderLeaflet({
    init_prods <- ids
    init_mat <- as.matrix(anim_df[, init_prods, drop = FALSE])
    storage.mode(init_mat) <- "numeric"
    
    ## custom popup for the piecharts
    popup_html <- make_popup_html(
      vals_mat    = init_mat,
      station_vec = anim_df$station_name,
      month_vec   = anim_df$month,
      id_names    = init_prods
    )
    
    pal_init <- unname(id_palette[colnames(init_mat)])  # aligned to column order
    
    # leaflet() %>%
    #   addTiles() %>%
    #   setView(lng = mean(stations$lon), lat = mean(stations$lat), zoom = 11) %>%
    map_base %>%
      addCircleMarkers(data = stations, lng = ~lon, lat = ~lat,
                       radius = 4, stroke = FALSE, fillOpacity = 1, fillColor = "grey") %>%
      addMinicharts(
        lng = stations$lon, lat = stations$lat,
        layerId = stations$station_name,
        chartdata = init_mat,
        type = "pie",
        time = anim_df$month,
        timeFormat = "%Y-%m",
        initialTime = months[1],
        legend = FALSE,                              # <-- important
        colorPalette = unname(id_palette[init_prods]),# <-- enough colors
        popup = popupArgs(html = popup_html),         # <-- custom popup
        width = width_all,
        height = height_all
        # width = 45, height = 45
      ) %>%
      addControl(
        html = HTML(make_legend_html(init_prods)),
        position = "topleft",
        layerId = "idLegend",
        className = "idLegendCtrl"
      ) %>%
      onRender("
      function(el, x){
        // prevent scroll-wheel on legend from zooming the map
        function fixLegend(){
          var node = el.querySelector('.idLegendCtrl .id-legend-scroll');
          if(!node) return false;
          if(window.L && L.DomEvent){
            L.DomEvent.disableScrollPropagation(node);
            L.DomEvent.disableClickPropagation(node);
          }
          return true;
        }
        if(!fixLegend()){
          var obs = new MutationObserver(function(){
            if(fixLegend()) obs.disconnect();
          });
          obs.observe(el, {childList:true, subtree:true});
        }
      }
    ")
  })
  # works but no custom popup
  #     addMinicharts(
  #       lng = stations$lon,
  #       lat = stations$lat,
  #       layerId = stations$station_name,
  #       chartdata = init_mat,
  #       type = "pie",
  #       colorPalette = pal_init,          # <-- THIS fixes the mismatch
  #       legend = FALSE,
  #       time = anim_df$month,
  #       width = 45,
  #       height = 45
  #     ) %>%
  #     addControl(
  #       html = HTML(make_legend_html(init_prods)),
  #       position = "topleft",
  #       layerId = "idLegend",
  #       className = "idLegendCtrl"
  #     ) %>%
  #     onRender("
  #     function(el, x){
  #       var node = el.querySelector('.idLegendCtrl .id-legend-scroll');
  #       if(node && window.L && L.DomEvent){
  #         L.DomEvent.disableScrollPropagation(node);
  #         L.DomEvent.disableClickPropagation(node);
  #       }
  #     }
  #   ")
  # })
  
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
  
  # observeEvent(list(input$prods, input$labels), {
  #
  #   # keep columns in stable order (ids order), not click-order
  #   prods <- ids[ids %in% input$prods]
  #
  #   if (length(prods) == 0) {
  #     data_mat <- matrix(0, nrow = nrow(anim_df), ncol = 1)
  #     colnames(data_mat) <- "none"
  #     pal <- "#999999"
  #   } else {
  #     data_mat <- as.matrix(anim_df[, prods, drop = FALSE])
  #     storage.mode(data_mat) <- "numeric"
  #     pal <- unname(id_palette[colnames(data_mat)])   # <- aligned to columns
  #   }
  #
  #   leafletProxy("map", session) %>%
  #     updateMinicharts(
  #       layerId = stations$station_name,
  #       chartdata = data_mat,
  #       maxValues = max(1, max(data_mat, na.rm = TRUE)),
  #       type = "pie",
  #       showLabels = input$labels,
  #       colorPalette = pal
  #     )
  #
  # }, ignoreInit = TRUE)
  
  observeEvent(list(input$prods, input$labels), {#, input$type
    
    # keep stable order (ids order), not click order
    prods <- ids[ids %in% input$prods]
    
    if (length(prods) == 0) {
      data_mat <- matrix(0, nrow = nrow(anim_df), ncol = 1)
      colnames(data_mat) <- "none"
      pal <- "#999999"
    } else {
      data_mat <- as.matrix(anim_df[, prods, drop = FALSE])
      storage.mode(data_mat) <- "numeric"
      pal <- unname(id_palette[colnames(data_mat)])  # aligned to columns
    }
    
    leafletProxy("map", session) %>%
      updateMinicharts(
        layerId = stations$station_name,
        chartdata = data_mat,
        maxValues = max(1, max(data_mat, na.rm = TRUE)),
        type = "pie", #input$type,
        showLabels = input$labels,
        colorPalette = pal,     # <-- THIS keeps colors consistent on updates
        legend = FALSE ,
        width = width_all,
        height = height_all
        # time = anim_df$month
        # IMPORTANT: do NOT pass time= here
      )
    
    # legend to match current selection/order:
    leafletProxy("map", session) %>%
      removeControl("idLegend") %>%
      addControl(
        html = HTML(make_legend_html(prods)),
        position = "topleft",
        layerId = "idLegend",
        className = "idLegendCtrl"
      )
    
  }, ignoreInit = TRUE)
  
  
  # scrollable legend but no mapping to piecharts ---------------------------
  # observe({
  #   prods <- input$prods
  #
  #   data <- if (length(prods) == 0) {
  #     matrix(0, nrow = nrow(chartdata_all), ncol = 1)
  #   } else {
  #     as.matrix(anim_df[, prods, drop = FALSE])
  #   }
  #   maxValue <- max(as.matrix(data))
  #
  #   leafletProxy("map", session) %>%
  #     updateMinicharts(
  #       layerId = stations$station_name,
  #       chartdata = data,
  #       maxValues = maxValue,
  #       type = if (ncol(data) < 2) "polar-area" else input$type,
  #       showLabels = input$labels,
  #       legend = FALSE,
  #       time = anim_df$month
  #     )
  # })
}

shinyApp(ui, server)
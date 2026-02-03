

# prep data ---------------------------------------------------------------

prep <- prep_minicharts_inputs(deployments, etn_monthyear_individual_sum)

stations <- prep$stations
anim_df  <- prep$anim_df
ids      <- prep$ids
months   <- prep$months
width_all  <- prep$width_all
height_all <- prep$height_all

chart_layer_id <- paste0(
  anim_df$station_name, "__", format(anim_df$month, "%Y%m")
)
layer_id_all <- anim_df$station_name  # length = nrow(anim_df)

# checks
stopifnot(length(chart_layer_id) == nrow(anim_df))

n_st <- nrow(stations)
n_m  <- length(months)

stopifnot(nrow(anim_df) == n_st * n_m)
stopifnot(all(table(anim_df$month) == n_st))


init_prods <- ids[1:min(30, length(ids))]

# ui ----------------------------------------------------------------------

mod_seabass_telemetry_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      height = 2000,
      selectInput(ns("prods"), "Select individuals", choices = NULL, selected = NULL, multiple = TRUE),
      fluidRow(
        column(6, actionButton(ns("prev_month"), "◀ Previous month", width = "100%")),
        column(6, actionButton(ns("next_month"), "Next month ▶", width = "100%"))
      ),
      selectInput(ns("type"), "Chart type", choices = c("pie", "bar", "polar-area", "polar-radius")),
      checkboxInput(ns("labels"), "Show values", value = FALSE),
      uiOutput(ns("month_summary"))
    ),
    mainPanel(
      leafletOutput(ns("map"), height = 1000)
    )
  )
}


# server ------------------------------------------------------------------

mod_seabass_telemetry_data_server <- function(
    id,
    deployments,
    etn_monthyear_individual_sum,
    base_map_fun = make_base_map   # lets you reuse your make_base_map()
) {
  moduleServer(id, function(input, output, session) {

    # palette (shuffle colors around)
    permute_spread <- function(n) {
      lo <- 1:ceiling(n/2)
      hi <- n:floor(n/2 + 1)
      as.vector(rbind(lo, hi))[1:n]
    }
    # define base palettes for the colors per individual
    base_pool <- c(
      RColorBrewer::brewer.pal(8, "Set2"),
      RColorBrewer::brewer.pal(12, "Paired"),
      RColorBrewer::brewer.pal(8, "Dark2"),
      RColorBrewer::brewer.pal(9, "Set1")
    )
    
    # exclude greys (because the contrast to the background map won't be high enough)
    drop_greys <- function(cols, min_chroma = 25) {
      rgb <- t(grDevices::col2rgb(cols)) / 255
      lab <- grDevices::convertColor(rgb, from = "sRGB", to = "Lab")
      a <- lab[, 2]; b <- lab[, 3]
      chroma <- sqrt(a^2 + b^2)
      cols[chroma >= min_chroma]
    }

    pool_filtered <- drop_greys(base_pool, min_chroma = 25)
    if (length(pool_filtered) < length(ids)) pool_filtered <- drop_greys(base_pool, min_chroma = 15)

    base_cols <- rep(pool_filtered, length.out = length(ids))
    perm <- permute_spread(length(ids))
    id_palette <- setNames(base_cols[perm], ids)

    make_legend_html <- function(id_vec) {
      if (length(id_vec) == 0) return("<div class='id-legend-scroll'><i>No individuals detected</i></div>")
      items <- paste0(
        "<div style='display:flex;align-items:center;gap:6px;margin:2px 0;'>",
        "<span style='width:12px;height:12px;display:inline-block;background:", id_palette[id_vec], ";'></span>",
        "<span>", id_vec, "</span>",
        "</div>",
        collapse = ""
      )
      paste0("<div class='id-legend-scroll'>", items, "</div>")
    }

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

    # initialize selectInput choices once
    observeEvent(TRUE, {
      updateSelectInput(session, "prods", choices = ids, selected = ids)
    }, once = TRUE)

    # ---- month state ----
    month_idx <- reactiveVal(1)
    current_month <- reactive(months[month_idx()])

    # ---- month summary UI ----
    output$month_summary <- renderUI({
      i <- month_idx()
      if (length(i) != 1L) i <- 1L
      i <- as.integer(i[1])
      if (is.na(i) || i < 1L) i <- 1L
      if (i > length(months)) i <- length(months)

      m <- months[i]

      df_m <- etn_monthyear_individual_sum %>%
        dplyr::mutate(month = as.Date(paste0(format(monthyear, "%Y-%m"), "-01"))) %>%
        dplyr::filter(month == m)

      if (nrow(df_m) == 0) {
        return(tagList(tags$hr(), tags$b(format(m, "%Y-%m")), tags$div("No detections for this month.")))
      }

      n_indiv <- df_m %>%
        dplyr::filter(n_detections > 0) %>%
        dplyr::summarise(n = dplyr::n_distinct(tag_serial_number), .groups = "drop") %>%
        dplyr::pull(n)
      if (length(n_indiv) == 0) n_indiv <- 0
      n_indiv <- as.integer(n_indiv[1]); if (is.na(n_indiv)) n_indiv <- 0

      total_det <- sum(df_m$n_detections, na.rm = TRUE)

      top_station_indiv <- df_m %>%
        dplyr::filter(n_detections > 0) %>%
        dplyr::group_by(station_name) %>%
        dplyr::summarise(n_indiv = dplyr::n_distinct(tag_serial_number), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(n_indiv)) %>%
        dplyr::slice_head(n = 1)

      top_indiv_name <- if (nrow(top_station_indiv) == 0) "—" else top_station_indiv$station_name[1]
      top_indiv_val  <- if (nrow(top_station_indiv) == 0) 0   else top_station_indiv$n_indiv[1]

      top_station_det <- df_m %>%
        dplyr::group_by(station_name) %>%
        dplyr::summarise(n_det = sum(n_detections, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(n_det)) %>%
        dplyr::slice_head(n = 1)

      top_det_name <- if (nrow(top_station_det) == 0) "—" else top_station_det$station_name[1]
      top_det_val  <- if (nrow(top_station_det) == 0) 0   else top_station_det$n_det[1]

      tagList(
        tags$hr(),
        tags$b(format(m, "%Y-%m")),
        tags$ul(
          tags$li(tagList("Number of individuals detected: ", tags$strong(n_indiv))),
          tags$li(tagList("Total detections this month: ", tags$strong(total_det))),
          tags$li(tagList("Station with most individuals: ", tags$strong(top_indiv_name), " (n = ", top_indiv_val, ")")),
          tags$li(tagList("Station with most detections: ", tags$strong(top_det_name), " (n = ", top_det_val, ")"))
        )
      )
    })

    # ---- Leaflet render ----
    output$map <- renderLeaflet({
      init_prods <- ids
      init_mat <- as.matrix(anim_df[, init_prods, drop = FALSE])
      storage.mode(init_mat) <- "numeric"

      popup_html <- make_popup_html(
        vals_mat    = init_mat,
        station_vec = anim_df$station_name,
        month_vec   = anim_df$month,
        id_names    = init_prods
      )

      base_map_fun() %>%
        leaflet::addCircleMarkers(
          data = stations, lng = ~lon, lat = ~lat,
          radius = 4, stroke = FALSE, fillOpacity = 1, fillColor = "grey"
        ) %>%
        leaflet.minicharts::addMinicharts(
          lng = anim_df$lon, lat = anim_df$lat,
          layerId = anim_df$station_name,
          chartdata = init_mat,
          type = "pie",
          time = anim_df$month,
          timeFormat = "%Y-%m",
          initialTime = months[1],
          legend = FALSE,
          colorPalette = unname(id_palette[init_prods]),
          popup = leaflet.minicharts::popupArgs(html = popup_html),
          width = width_all,
          height = height_all
        ) %>%
        leaflet::addControl(
          html = htmltools::HTML(make_legend_html(init_prods)),
          position = "topleft",
          layerId = "idLegend",
          className = "idLegendCtrl"
        ) %>%
        htmlwidgets::onRender("
          function(el, x){
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

    # ---- month buttons ----
    observeEvent(input$prev_month, {
      i <- month_idx()
      month_idx(if (i <= 1) length(months) else i - 1)
    })

    observeEvent(input$next_month, {
      i <- month_idx()
      month_idx(if (i >= length(months)) 1 else i + 1)
    })

    observeEvent(month_idx(), {
      leafletProxy("map", session = session) %>%
        leaflet.minicharts::updateMinicharts(
          layerId = stations$station_name,
          initialTime = months[month_idx()]
        )
    }, ignoreInit = TRUE)

    # ---- update selection / labels ----
    observeEvent(list(input$prods, input$labels), {
      prods <- ids[ids %in% input$prods]
      
      if (length(prods) == 0) {
        data_mat <- matrix(0, nrow = nrow(anim_df), ncol = 1)
        colnames(data_mat) <- "none"
        pal <- "#999999"
      } else {
        data_mat <- as.matrix(anim_df[, prods, drop = FALSE])
        storage.mode(data_mat) <- "numeric"
        pal <- unname(id_palette[colnames(data_mat)])
      }
      
      leafletProxy("map", session = session) %>%
        leaflet.minicharts::updateMinicharts(
          layerId = anim_df$station_name,
          chartdata = data_mat,
          time = anim_df$month,
          timeFormat = "%Y-%m",
          initialTime = months[month_idx()],
          maxValues = max(1, max(data_mat, na.rm = TRUE)),
          type = "pie",
          showLabels = input$labels,
          colorPalette = pal,
          legend = FALSE,
          width = width_all,
          height = height_all
        )
      
      
      
      leafletProxy("map", session = session) %>%
        removeControl("idLegend") %>%
        addControl(
          html = htmltools::HTML(make_legend_html(prods)),
          position = "topleft",
          layerId = "idLegend",
          className = "idLegendCtrl"
        )
    }, ignoreInit = TRUE)
    
    # observeEvent(list(input$prods, input$labels), {
    #   prods <- ids[ids %in% input$prods]
    # 
    #   if (length(prods) == 0) {
    #     data_mat <- matrix(0, nrow = nrow(anim_df), ncol = 1)
    #     colnames(data_mat) <- "none"
    #     pal <- "#999999"
    #   } else {
    #     data_mat <- as.matrix(anim_df[, prods, drop = FALSE])
    #     storage.mode(data_mat) <- "numeric"
    #     pal <- unname(id_palette[colnames(data_mat)])
    #   }
    # 
    #   leafletProxy("map", session = session) %>%
    #     leaflet.minicharts::updateMinicharts(
    #       layerId = stations$station_name,
    #       chartdata = data_mat,
    #       maxValues = max(1, max(data_mat, na.rm = TRUE)),
    #       type = "pie",
    #       showLabels = input$labels,
    #       colorPalette = pal,
    #       legend = FALSE,
    #       width = width_all,
    #       height = height_all
    #     )
    # 
    #   leafletProxy("map", session = session) %>%
    #     leaflet::removeControl("idLegend") %>%
    #     leaflet::addControl(
    #       html = htmltools::HTML(make_legend_html(prods)),
    #       position = "topleft",
    #       layerId = "idLegend",
    #       className = "idLegendCtrl"
    #     )
    # }, ignoreInit = TRUE)
  })
}


# mod_seabass_telemetry_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     leafletOutput(ns("data_map"), height = 700),
#     DTOutput(ns("acoustic_telemetry"))
#   )
# }
# 
# mod_seabass_telemetry_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     output$data_map <- renderLeaflet({ make_base_map() })
#     output$acoustic_telemetry <- renderDT({ datasets::cars })
#   })
# }

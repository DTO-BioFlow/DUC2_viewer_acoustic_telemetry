library(shiny)
library(DT)
library(leaflet)
library(glue)

source("R/config.R")
source("R/maps_env.R")
source("R/maps_acoustic.R")
source("R/module_home.R")
source("R/module_seabass.R")
source("R/module_porpoise.R")

ui <- fluidPage(
  tags$head(
    tags$style(HTML(glue::glue("
      :root {{
        --blue-light: {blue_light};
        --blue-medium: {blue_medium};
        --blue-dark: {blue_dark};
      }}
    "))),
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  
  titlePanel("DUC2: Impact from offshore infrastructures on marine life"),
  
  mainPanel(
    width = 12,
    tags$div(
      class = "top-tabs container-fluid",
      
      tags$a(
        href = bioflow_url,
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
        
        tabPanel("Home", style = "font-size: 16px;", mod_home_ui("home")),
        
        tabPanel(
          title = tagList(
            tags$img(src = "D_labrax_phylopic_CC0.png", height = "24px",
                     style = "vertical-align:middle; margin-right:8px;"),
            tags$span("European seabass", style = "font-size: 16px; vertical-align:middle;")
          ),
          class = "lower-level-tabs",
          mod_seabass_ui("seabass")
        ),
        
        tabPanel(
          title = tagList(
            tags$img(src = "P_phocoena_phylopic_CC0.png", height = "24px",
                     style = "vertical-align:middle; margin-right:8px;"),
            tags$span("Harbour porpoise", style = "font-size: 16px; vertical-align:middle;")
          ),
          class = "lower-level-tabs",
          mod_porpoise_ui("porpoise")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  mod_home_server("home")
  mod_seabass_server("seabass")
  mod_porpoise_server("porpoise")
}

shinyApp(ui, server)


# library(shiny)
# library(DT)
# library(leaflet)
# library(glue)
# 
# source("R/config.R")
# source("R/maps.R")
# source("R/mod_home.R")
# source("R/mod_seabass.R")
# source("R/mod_porpoise.R")
# 
# ui <- fluidPage(
#   tags$head(
#     # Define theme values once; CSS uses these variables
#     tags$style(HTML(glue::glue("
#       :root {{
#         --blue-light: {blue_light};
#         --blue-medium: {blue_medium};
#         --blue-dark: {blue_dark};
#       }}
#     "))),
#     # All the real styling lives here
#     tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
#   ),
#   
#   titlePanel("DUC2: Impact from offshore infrastructures on marine life"),
#   
#   mainPanel(
#     width = 12,
#     tags$div(
#       class = "top-tabs container-fluid",
#       
#       # RIGHT-SIDE LOGO (clickable)
#       tags$a(
#         href = bioflow_url,
#         target = "_blank",
#         rel = "noopener",
#         class = "top-tabs-logo",
#         tags$img(
#           src = "Logo_BIO-Flow2023_Final_Positive.png",
#           height = "42px",
#           alt = "DTO-Bioflow"
#         )
#       ),
#       
#       tabsetPanel(
#         id = "tabsetPanelID",
#         type = "tabs",
#         
#         tabPanel(
#           "Home",
#           style = "font-size: 16px;",
#           mod_home_ui("home")
#         ),
#         
#         tabPanel(
#           title = tagList(
#             tags$img(
#               src = "D_labrax_phylopic_CC0.png",
#               height = "24px",
#               style = "vertical-align:middle; margin-right:8px;"
#             ),
#             tags$span("European seabass", style = "font-size: 16px; vertical-align:middle;")
#           ),
#           class = "lower-level-tabs",
#           mod_seabass_ui("seabass")
#         ),
#         
#         tabPanel(
#           title = tagList(
#             tags$img(
#               src = "P_phocoena_phylopic_CC0.png",
#               height = "24px",
#               style = "vertical-align:middle; margin-right:8px;"
#             ),
#             tags$span("Harbour porpoise", style = "font-size: 16px; vertical-align:middle;")
#           ),
#           class = "lower-level-tabs",
#           mod_porpoise_ui("porpoise")
#         )
#       )
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   mod_home_server("home")
#   mod_seabass_server("seabass")
#   mod_porpoise_server("porpoise")
# }
# 
# shinyApp(ui, server)

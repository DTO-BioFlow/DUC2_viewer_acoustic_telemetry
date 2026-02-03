mod_porpoise_ui <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel("PAM dashboard", DTOutput(ns("PAM_dashboard"))),
    tabPanel("PAM data", DTOutput(ns("PAM_data"))),
    tabPanel("Habitat suitability (Marco-Bolo)", DTOutput(ns("HSM_porpoise")))
  )
}

mod_porpoise_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$PAM_dashboard <- renderDT({
      datasets::cars
    })
    
    output$PAM_data <- renderDT({
      datasets::cars
    })
    
    output$HSM_porpoise <- renderDT({
      head(datasets::cars, 3)
    })
  })
}

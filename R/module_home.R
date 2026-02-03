mod_home_ui <- function(id) {
  ns <- NS(id)
  
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
          href = bioflow_duc2_url,
          target = "_blank",
          rel = "noopener",
          class = "btn btn-primary d-flex align-items-center justify-content-center",
          style = glue::glue("margin-bottom:25px;background-color:{blue_medium}; color:white;")
        ),
        tags$br(),
        tags$a(
          tags$img(
            src = "Logo_BIO-Flow2023_Final_Negative.png",
            height = "50px",
            style = "vertical-align:middle"
          ),
          href = bioflow_url,
          target = "_blank",
          rel = "noopener",
          class = "btn btn-primary bioflow-btn bioflow-btn--logo"
        )
      )
    )
  )
}

mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic yet
  })
}

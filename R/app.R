# Agricultural Land Suitability Analysis (ALSA) Shiny App
library(shiny)

ALSA_app <- function(...) {
  ui <- navbarPage(
    "Agricultural Land Suitability Analysis (ALSA)",

    navbarMenu(
      "Upload Data",
      tabPanel(
        "Soil and Climate Data",
        soilClimateDataUI("soilClimateData")
      ),
      tabPanel(
        "Crop Suitability Parameters",
        cropSuitabilityParamsUI("cropSuitabilityParams")
      )
    )
  )

  server <- function(input, output, session) {
    soilClimateDataServer("soilClimateData")
    cropSuitabilityParamsServer("cropSuitabilityParams")
  }

  shinyApp(ui, server, ...)
}

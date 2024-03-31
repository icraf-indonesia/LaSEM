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
      ),
      tabPanel(
        "Intervention Lookup",
        interventionLookupUI("interventionLookup")
      )
    ),
    navbarMenu(
      "Analysis",
      tabPanel("Suitability Analysis",
               suitabilityAnalysisUI("suitabilityAnalysis"))

    )

  )

  server <- function(input, output, session) {

    # Create reactive values to store submitted data
    submittedData <- reactiveValues(
      soilClimateData = NULL,
      cropParams = NULL,
      interventionLookup = NULL,
      siteLocation = NULL,
      cropName = NULL
    )

    soilClimateDataServer("soilClimateData", submittedData)
    cropSuitabilityParamsServer("cropSuitabilityParams", submittedData)
    interventionLookupServer("interventionLookup", submittedData)



    suitabilityAnalysisServer(
      "suitabilityAnalysis",
      submittedData
    )
  }

  shinyApp(ui, server, ...)
}

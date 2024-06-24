# Land Suitability Evaluation Module (LaSEM) Shiny App

library(shiny)

#' Land Suitability Evaluation Module (LaSEM) Shiny App
#'
#' This function creates a Shiny application for conducting agricultural land suitability analysis.
#'
#' @param ... Additional arguments to pass to the Shiny app.
#'
#' @importFrom shiny navbarPage navbarMenu tabPanel reactiveValues shinyApp
#' @export
LaSEM_app <- function(...) {
  ui <- navbarPage(
    "Land Suitability Evaluation Module (LaSEM)",
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

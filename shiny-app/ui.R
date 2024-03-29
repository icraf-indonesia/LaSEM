library(shiny)

ui <- navbarPage(
  "Agricultural Land Suitability Analysis (ALSA)",

  navbarMenu("Upload Data",
             tabPanel("Soil and Climate Data",
                      soilClimateDataUI("soilClimateData")
             ),
             tabPanel("Crop Suitability Parameters",
                      cropSuitabilityParamsUI("cropSuitabilityParams")
             )
  )
)

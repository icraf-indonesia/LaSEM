library(shiny)

server <- function(input, output, session) {
  soilClimateDataServer("soilClimateData")
  cropSuitabilityParamsServer("cropSuitabilityParams")
}

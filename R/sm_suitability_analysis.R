#' Suitability Analysis Module
#'
#' This module provides a user interface and server logic for performing suitability analysis
#' based on soil and climate data, crop parameters, and intervention lookup data. It displays
#' the suitability map, suitability by factors, suitability polygon data table, and provides
#' an option to download the suitability analysis results.
#'
#' @param id The module ID.
#'
#' @importFrom shiny NS tagList tabsetPanel tabPanel plotOutput  downloadButton
#' @importFrom shiny  renderPlot downloadHandler
#' @importFrom shiny req reactive moduleServer
#' @importFrom leaflet leaflet addTiles addPolygons addLegend
#' @importFrom leaflet leafletOutput renderLeaflet
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom sf write_sf
#' @importFrom terra writeRaster
#' @importFrom utils zip
#'
#' @export
suitabilityAnalysisUI <- function(id) {
  tagList(
    tabsetPanel(
      tabPanel("Suitability Map", leafletOutput(NS(id, "suitabilityMap"))),
      tabPanel("Suitability by Factors", plotOutput(NS(id, "suitabilityFactors"))),
      tabPanel("Suitability Polygon", DTOutput(NS(id, "suitabilityPolygon"))),
      tabPanel("Download Results", downloadButton(NS(id, "downloadResults"), "Download Results"))
    )
  )
}

#' @rdname suitabilityAnalysisUI
#' @importFrom dplyr left_join mutate case_when
#' @importFrom leaflet colorFactor
#'
#' @export
suitabilityAnalysisServer <- function(id, submittedData) {
  moduleServer(id, function(input, output, session) {
    # Perform suitability analysis using the submitted data
    suitabilityResults <- reactive({
      req(submittedData$soilClimateData, submittedData$cropParams, submittedData$interventionLookup)

      perform_suitability_analysis(
        harmonised_rasters = submittedData$soilClimateData,
        suitability_parameter = submittedData$cropParams,
        lookup_intervention = submittedData$interventionLookup
      )
    })

    # Display the suitability map
    output$suitabilityMap <- leaflet::renderLeaflet({
      req(suitabilityResults())
      suitability_map <- suitabilityResults()$suitability_polygon

      # Create a color palette for mapping the 'suitability' variable
      colors <- colorFactor(palette = c("olivedrab", "olivedrab2", "orange", "red"),
                            levels = c("S1", "S2", "S3", "N"), ordered = FALSE)

      leaflet(suitability_map) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~colors(suitability),
          fillOpacity = 1,
          color = "grey",
          weight = 0.1,
          smoothFactor = 1
        ) %>%
        addLegend(
          "bottomright",
          pal = colors,
          values = ~suitability,
          title = "Suitability Level",
          opacity = 0.7
        )
    })

    # Display the suitability by factors
    output$suitabilityFactors <- renderPlot({
      req(suitabilityResults())
      suitability_by_factors <- suitabilityResults()$suitability_by_factors
      plot(suitability_by_factors)
    })

    # Display the suitability polygon data table
    output$suitabilityPolygon <- renderDT({
      req(suitabilityResults())
      suitability_polygon <- suitabilityResults()$suitability_polygon
      datatable(suitability_polygon)
    })

    # Provide an option to download the suitability analysis results
    output$downloadResults <- downloadHandler(
      filename = function() {
        paste("suitability_results", Sys.Date(), ".zip", sep = "_")
      },
      content = function(file) {
        # Save the suitability analysis results to a temporary directory
        tempdir <- tempdir()

        # Save the suitability map, polygon, and other results to the temporary directory
        suitability_map <- suitabilityResults()$suitability_polygon
        write_sf(suitability_map, file.path(tempdir, "suitability_map.shp"))

        suitability_by_factors <- suitabilityResults()$suitability_by_factors
        writeRaster(suitability_by_factors, file.path(tempdir, "suitability_by_factors.tif"))

        # Zip the temporary directory and write it to the specified file
        zip(file, tempdir)
      }
    )
  })
}

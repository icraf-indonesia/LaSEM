#' Suitability Analysis Module
#'
#' This module provides a user interface and server logic for performing suitability analysis
#' based on soil and climate data, crop parameters, and intervention lookup data. It displays
#' the suitability map, suitability by factors, suitability polygon data table, and provides
#' an option to download the suitability analysis results.
#'
#' @param id The module ID.
#' @param submittedData reactive values to store submitted data
#'
#' @importFrom shiny NS tagList tabsetPanel tabPanel plotOutput  downloadButton
#' @importFrom shiny  renderPlot downloadHandler fluidRow column renderText
#' @importFrom shiny req reactive moduleServer sliderInput textOutput updateSliderInput
#' @importFrom leaflet leaflet addTiles addPolygons addLegend addProviderTiles labelOptions
#' @importFrom leaflet leafletOutput renderLeaflet hideGroup addLayersControl layersControlOptions
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom sf write_sf st_as_sfc st_bbox
#' @importFrom terra writeRaster
#' @importFrom shinycssloaders withSpinner
#' @importFrom utils zip
#' @importFrom htmltools h2 h4 p
#'
#' @export
suitabilityAnalysisUI <- function(id) {
  tagList(
    fluidRow(
      column(12,
             h2(textOutput(NS(id, "title")), style = "font-weight: bold;"),
             h4(textOutput(NS(id, "subtitle")), style = "color: #888;")
      )
    ),
    tabsetPanel(
      tabPanel("Suitability Map", withSpinner(leafletOutput(NS(id, "suitabilityMap")))),
      tabPanel("Suitability Map by Factors",
               fluidRow(
                 column(12,
                        sliderInput(NS(id, "factorSlider"), "Select Factor:", min = 1, max = 1, value = 1, step = 1),
                        plotOutput(NS(id, "suitabilityFactorPlot"))
                 )
               )
      ),
      tabPanel("Attribute Table", DTOutput(NS(id, "suitabilityPolygon"))),
      tabPanel("Download Results",
               downloadButton(NS(id, "downloadShapefile"), "Download Shapefile"),
               downloadButton(NS(id, "downloadRaster"), "Download Raster"),
               downloadButton(NS(id, "downloadTable"), "Download Attribute Table"))
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
    # Check if the required data is available
    is_data_available <- reactive({
      all_data_available <- !is.null(submittedData$soilClimateData) &&
        !is.null(submittedData$cropParams) &&
        !is.null(submittedData$interventionLookup) &&
        !is.null(submittedData$siteLocation) &&
        !is.null(submittedData$cropName)

      if (!all_data_available) {
        showNotification(
          ui = tagList(
            h4("Incomplete Data Upload"),
            p("Please complete the upload data tabs to perform the suitability analysis.")
          ),
          duration = NULL,
          type = "error",
          closeButton = TRUE,
          id = "incomplete-data-notification"
        )
      }

      all_data_available
    })

    # Expose is_data_available to the UI
    output$is_data_available <- reactive(is_data_available())

    # Render the title and subtitle
    output$title <- renderText({
      req(submittedData$cropName)
      paste("Land Suitability Evaluation of", submittedData$cropName)
    })

    output$subtitle <- renderText({
      req(submittedData$siteLocation)
      paste("in", submittedData$siteLocation)
    })

    # Perform suitability analysis using the submitted data
    suitabilityResults <- reactive({
      req(is_data_available())
      perform_suitability_analysis(
        harmonised_rasters = submittedData$soilClimateData,
        suitability_parameter = submittedData$cropParams,
        lookup_intervention = submittedData$interventionLookup
      )
    })

    # Display the suitability map
    output$suitabilityMap <- renderLeaflet({
      req(suitabilityResults())
      suitability_polygon <- suitabilityResults()$suitability_polygon

      # Construct HTML strings for labels on the map with suitability information
      suitability_polygon$label_content <- with(
        suitability_polygon,
        paste0(
          "<strong>Suitability Actual:</strong> ",
          suitability,
          "<br><strong>Max Potential:</strong> ",
          suitability_potential_high,
          "<br><strong>Primary Limiting Factor:</strong> ",
          limiting_factor_actual,
          "<br><strong>Secondary Limiting Factor:</strong> ",
          limiting_factor_potential
        )
      ) |> lapply(htmltools::HTML) # Convert labels to HTML

      # Create a color palette for mapping the 'suitability' variable
      colors <- colorFactor(palette = c("olivedrab", "olivedrab2", "orange", "red"),
                            levels = c("S1", "S2", "S3", "N"), ordered = FALSE)

      bbox_poly <- st_bbox(suitability_polygon) |>
        st_as_sfc()

      leaflet(suitability_polygon) |>
        addProviderTiles("CyclOSM") |>
        addPolygons(
          fillColor = ~ colors(suitability),
          fillOpacity = 1,
          color = "grey",
          weight = 0.1,
          smoothFactor = 1,
          label = ~ label_content,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", "color" = "black"),
            textsize = "13px",
            direction = "auto"
          ),
          group = "Aktual"
        ) |>
        addPolygons(
          fillColor = ~ colors(suitability_potential_low),
          fillOpacity = 1,
          color = "grey",
          weight = 0.1,
          smoothFactor = 1,
          label = ~ label_content,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", "color" = "black"),
            textsize = "13px",
            direction = "auto"
          ),
          group = "Low Intervention"
        ) |>
        addPolygons(
          fillColor = ~ colors(suitability_potential_med),
          fillOpacity = 1,
          color = "grey",
          weight = 0.1,
          smoothFactor = 1,
          label = ~ label_content,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", "color" = "black"),
            textsize = "13px",
            direction = "auto"
          ),
          group = "Medium Intervention"
        ) |>
        addPolygons(
          fillColor = ~ colors(suitability_potential_high),
          fillOpacity = 1,
          color = "grey",
          weight = 0.1,
          smoothFactor = 1,
          label = ~ label_content,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", "color" = "black"),
            textsize = "13px",
            direction = "auto"
          ),
          group = "High Intervention"
        ) |>
        addLegend(
          "bottomright",
          pal = colors,
          values = ~ factor(c(suitability_potential_low, suitability_potential_med, suitability_potential_high),
                            levels = c("S1", "S2", "S3", "N")),
          title = "Suitability Level",
          opacity = 0.7
        ) |>
        addLayersControl(
          baseGroups = c("Aktual", "Low Intervention", "Medium Intervention", "High Intervention"),
          options = layersControlOptions(collapsed = FALSE)
        ) |>
        hideGroup(c("Low Intervention", "Medium Intervention", "High Intervention"))
    })

    # Render individual factor plots
    output$suitabilityFactorPlot <- renderPlot({
      req(suitabilityResults())
      suitability_by_factors <- suitabilityResults()$suitability_by_factors
      num_factors <- terra::nlyr(suitability_by_factors)

      # Update the slider input max value based on the number of factors
      updateSliderInput(session, "factorSlider", max = num_factors)

      # Get the selected factor index from the slider input
      selected_factor <- input$factorSlider

      # Plot the selected factor
      plot(suitability_by_factors[[selected_factor]], main = names(suitability_by_factors)[selected_factor])
    })

    # Display the suitability polygon data table
    output$suitabilityPolygon <- renderDT({
      req(suitabilityResults())
      suitability_polygon <- suitabilityResults()$suitability_polygon
      datatable(suitability_polygon)
    })

    # Provide options to download different sets of files
    output$downloadShapefile <- downloadHandler(
      filename = function() {
        paste("suitability_map", Sys.Date(), ".zip", sep = "_")
      },
      content = function(file) {
        # Save the suitability map to a temporary directory
        tempdir <- tempdir()
        browser()
        suitability_map <- suitabilityResults()$suitability_polygon
        st_write(suitability_map, file.path(tempdir, "suitability_map.shp"),
                 append = FALSE)

        # Zip the temporary directory and write it to the specified file
        zip(file, tempdir)
      }
    )

    output$downloadRaster <- downloadHandler(
      filename = function() {
        paste("suitability_by_factors", Sys.Date(), ".tif", sep = "_")
      },
      content = function(file) {
        suitability_by_factors <- suitabilityResults()$suitability_by_factors
        writeRaster(suitability_by_factors, file, overwrite = TRUE)
      }
    )

    output$downloadTable <- downloadHandler(
      filename = function() {
        paste("suitability_attr", Sys.Date(), ".csv", sep = "_")
      },
      content = function(file) {
        suitability_attr <- suitabilityResults()$suitability_attr
        write_csv(suitability_attr, file)
      }
    )
  })
}

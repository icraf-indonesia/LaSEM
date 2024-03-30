#' Soil and Climate Data Module UI
#'
#' This module provides a user interface for uploading and visualizing soil and climate data.
#'
#' @param id The ID of the module, used to create namespaces for the UI elements.
#' @importFrom shiny tagList sidebarLayout sidebarPanel fileInput NS checkboxInput actionButton mainPanel tabsetPanel tabPanel plotOutput
#' @importFrom DT DTOutput
#' @return A tagList containing the UI elements for the Soil and Climate Data module.
#'
#' @export
soilClimateDataUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(NS(id, "soilClimateData"), "Upload Lookup Table of Soil and Climate Data (CSV)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        checkboxInput(NS(id, "editableTable"), "Editable Table", value = FALSE),
        actionButton(NS(id, "visualizeRasters"), "Visualize Rasters"),
        actionButton(NS(id, "submitSoilClimateData"), "Submit Soil Climate Data")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data Table", DTOutput(NS(id, "dataTable"))),
          tabPanel("Raster Visualization", plotOutput(NS(id, "rasterPlot")))
        )
      )
    )
  )
}

#' Soil and Climate Data Module Server
#'
#' This module server handles the logic for uploading, processing, and visualizing soil and climate data.
#'
#' @param id The ID of the module, used to create namespaces for the server elements.
#'
#' @return The server logic for the Soil and Climate Data module.
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent req showNotification
#' @importFrom tools file_ext
#' @importFrom DT datatable renderDT
#' @importFrom dplyr filter
#' @importFrom readr read_csv
#' @importFrom terra plot
#'
#' @export
soilClimateDataServer <- function(id, submittedData) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression for the uploaded CSV file
    csvData <- reactive({
      req(input$soilClimateData)

      # Validate and read the uploaded file
      ext <- tools::file_ext(input$soilClimateData$name)
      if (ext != "csv") {
        showNotification("Please upload a CSV file.", type = "error")
        return(NULL)
      }

      read_csv(input$soilClimateData$datapath)
    })

    # Render the data table
    output$dataTable <- renderDT({
      req(csvData())
      datatable(csvData(), editable = input$editableTable)
    })

    # Observe changes in the editable table and update the data
    observeEvent(input$dataTable_cell_edit, {
      info <- input$dataTable_cell_edit
      csvData()[info$row, info$col] <<- info$value
    })

    # Reactive expression for filtered climate_soil_data
    filteredData <- reactive({
      req(csvData())
      csvData() %>%
        dplyr::filter(availability %in% "Yes")
    })

    # Reactive expression for loaded rasters
    loadedRasters <- eventReactive(input$visualizeRasters, {
      req(filteredData())
      withProgress(message = "Loading rasters", {
        read_raster_files(filteredData())
      })
    })

    # Reactive expression for stacked rasters
    stackedRasters <- reactive({
      req(loadedRasters())
      stack_raster_layers(loadedRasters(), filteredData()$parameter_name)
    })

    # Render the raster plot
    output$rasterPlot <- renderPlot({
      req(stackedRasters())
      terra::plot(stackedRasters())
    })

    observeEvent(input$submitSoilClimateData, {
      submittedData$soilClimateData <- stackedRasters()
      showNotification("Soil Climate Data submitted successfully!", type = "message")
    })

  })
}

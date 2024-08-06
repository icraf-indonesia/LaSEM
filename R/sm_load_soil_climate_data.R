#' Soil and Climate Data Module UI
#'
#' This module provides a user interface for uploading and visualizing soil and climate data.
#'
#' @param id The ID of the module, used to create namespaces for the UI elements.
#' @importFrom shiny tagList sidebarLayout sidebarPanel fileInput NS moduleServer
#' @importFrom shiny uiOutput checkboxInput actionButton mainPanel tabsetPanel tabPanel
#' @importFrom shiny plotOutput textInput
#' @importFrom DT DTOutput
#' @importFrom utils read.csv
#' @return A tagList containing the UI elements for the Soil and Climate Data module.
#'
#' @export
soilClimateDataUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(NS(id, "siteLocation"), "Site Location", placeholder = "Enter the location or area of analysis"),
        fileInput(NS(id, "soilClimateData"), "Upload Lookup Table of Soil and Climate Data (CSV)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        checkboxInput(NS(id, "editableTable"), "Editable Table", value = FALSE),
        actionButton(NS(id, "submitSoilClimateData"), "Submit Soil Climate Data"),
        uiOutput(NS(id, "siteLocationCheck"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data Table", DTOutput(NS(id, "dataTable"))),
          tabPanel("Preview Maps", plotOutput(NS(id, "rasterPlot")))
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
#' @param submittedData reactive values to store submitted data
#'
#' @return The server logic for the Soil and Climate Data module.
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent
#' @importFrom shiny req showNotification withProgress renderPlot eventReactive
#' @importFrom tools file_ext
#' @importFrom DT datatable renderDT
#' @importFrom dplyr filter
#' @importFrom readr read_csv
#' @importFrom terra plot
#'
#' @export
soilClimateDataServer <- function(id, submittedData) {
  moduleServer(id, function(input, output, session) {
    # Create a reactive value to store the soil climate data
    csvDataReactive <- reactiveVal(NULL)

    # Observe changes in the uploaded CSV file
    observeEvent(input$soilClimateData, {
      # Validate and read the uploaded file
      ext <- tools::file_ext(input$soilClimateData$name)
      if (ext != "csv") {
        showNotification("Please upload a CSV file.", type = "error")
        return(NULL)
      }

      csvData <- read.csv(input$soilClimateData$datapath, stringsAsFactors = FALSE)
      csvDataReactive(csvData)
    })

    # Render the data table
    output$dataTable <- renderDT({
      datatable(csvDataReactive(), editable = input$editableTable)
    })

    # Observe changes in the editable table and update the data
    observeEvent(input$dataTable_cell_edit, {
      info <- input$dataTable_cell_edit
      csvData <- csvDataReactive()
      csvData[info$row, info$col] <- info$value
      csvDataReactive(csvData)
    })

    # Create a reactive expression for filtered soil climate data
    filteredData <- reactive({
      csvData <- csvDataReactive()
      if (!is.null(csvData)) {
        csvData %>%
          dplyr::filter(availability %in% "Yes")
      }
    })

    # Reactive expression for loaded rasters
    loadedRasters <- reactive({
      req(filteredData())
      read_raster_files(filteredData())
    })

    # Reactive expression for stacked rasters
    stackedRasters <- reactive({
      req(loadedRasters())
      stack_raster_layers(loadedRasters(), filteredData()$parameter_name)
    })

    # Render the raster plot
    output$rasterPlot <- renderPlot({
      req(stackedRasters())
      plot(stackedRasters())
    })

    # Observe the submit button click event
    observeEvent(input$submitSoilClimateData, {
      req(stackedRasters())
      if (!is.null(csvDataReactive())) {
        submittedData$soilClimateData <- stackedRasters()
        submittedData$siteLocation <- input$siteLocation
        print(submittedData$siteLocation)
        print(submittedData$soilClimateData)
        showNotification("Soil Climate Data submitted successfully!", type = "message")
        updateTabsetPanel(session, "tabset", selected = "Preview Maps")
      } else {
        showNotification("Please upload a valid CSV file.", type = "error")
      }
    })
  })
}

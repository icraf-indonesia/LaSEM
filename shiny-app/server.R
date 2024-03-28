library(shiny)
library(DT)
library(terra)
library(ALSA)
library(dplyr)

server <- function(input, output, session) {
  # Reactive expression for the uploaded CSV file
  csvData <- reactive({
    req(input$csvFile)
    read.csv(input$csvFile$datapath, stringsAsFactors = FALSE)
  })

  # Render the data table
  output$dataTable <- renderDT({
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
    plot(stackedRasters())
  })
}

#' Crop Suitability Parameters Module UI
#'
#' This module provides a user interface for uploading and managing crop suitability parameters.
#'
#' @param id The ID of the module, used to create namespaces for the UI elements.
#'
#' @return A tagList containing the UI elements for the Crop Suitability Parameters module.
#'
#' @export
cropSuitabilityParamsUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(NS(id, "cropName"), "Crop Name", placeholder = "Enter the name of the crop"),
        fileInput(NS(id, "cropSuitabilityData"), "Upload Crop Suitability Parameters (CSV)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        checkboxInput(NS(id, "editableTable"), "Editable Table", value = FALSE),
        actionButton(NS(id, "submitCropParams"), "Submit Crop Parameters"),
        uiOutput(NS(id, "cropNameCheck"))
      ),
      mainPanel(
        DTOutput(NS(id, "cropSuitabilityTable"))
      )
    )
  )
}


#' Crop Suitability Parameters Module Server
#'
#' This module server handles the logic for uploading and managing crop suitability parameters.
#'
#' @param id The ID of the module, used to create namespaces for the server elements.
#'
#' @return The server logic for the Crop Suitability Parameters module.
#'
#' @importFrom shiny moduleServer reactive observeEvent
#' @importFrom tools file_ext
#' @importFrom DT renderDT
#' @importFrom readr read_csv
#'
#' @export
cropSuitabilityParamsServer <- function(id, submittedData) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression for the uploaded crop suitability parameters CSV file
    cropSuitabilityData <- reactive({
      req(input$cropSuitabilityData)

      # Validate and read the uploaded file
      ext <- tools::file_ext(input$cropSuitabilityData$name)
      if (ext != "csv") {
        showNotification("Please upload a CSV file.", type = "error")
        return(NULL)
      }

      read_csv(input$cropSuitabilityData$datapath)
    })

    # Render the crop suitability parameters data table
    output$cropSuitabilityTable <- renderDT({
      req(cropSuitabilityData())
      datatable(cropSuitabilityData(), editable = input$editableTable)
    })

    # Observe changes in the editable table and update the data
    observeEvent(input$cropSuitabilityTable_cell_edit, {
      info <- input$cropSuitabilityTable_cell_edit
      cropSuitabilityData()[info$row, info$col] <<- info$value
    })

    # Observe submit button clicks in module servers
    observeEvent(input$submitCropParams, {
      submittedData$cropParams <- cropSuitabilityData()
    })

    # Validate crop name input
    cropNameValid <- reactive({
      input$cropName != "" && !grepl("^\\d", input$cropName)
    })

    output$cropNameCheck <- renderUI({
      if (cropNameValid()) {
        icon("check", class = "text-success")
      } else {
        ""
      }
    })

    observeEvent(input$submitCropParams, {
      if (cropNameValid()) {
        submittedData$cropParams <- cropSuitabilityData()
        submittedData$cropName <- input$cropName
        showNotification("Crop Parameters submitted successfully!", type = "message")
      } else {
        showNotification("Please enter a valid crop name.", type = "error")
      }
    })

  })
}

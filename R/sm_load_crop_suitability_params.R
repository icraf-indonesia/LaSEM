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
        fileInput(NS(id, "cropSuitabilityData"), "Upload Crop Suitability Parameters (CSV)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        checkboxInput(NS(id, "editableTable"), "Editable Table", value = FALSE),
        actionButton(NS(id, "submitCropParams"), "Submit Crop Parameters")
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

  })
}

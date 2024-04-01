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
#' @param submittedData reactive values to store submitted data
#'
#' @return The server logic for the Crop Suitability Parameters module.
#'
#' @importFrom shiny moduleServer reactive observeEvent reactiveVal
#' @importFrom shiny showNotification
#' @importFrom tools file_ext
#' @importFrom DT renderDT datatable
#' @importFrom readr read_csv
#'
#' @export
cropSuitabilityParamsServer <- function(id, submittedData) {
  moduleServer(id, function(input, output, session) {
    # Create a reactive value to store the crop suitability data
    cropSuitabilityDataReactive <- reactiveVal(NULL)

    # Observe changes in the uploaded crop suitability parameters CSV file
    observeEvent(input$cropSuitabilityData, {
      # Validate and read the uploaded file
      ext <- file_ext(input$cropSuitabilityData$name)
      if (ext != "csv") {
        showNotification("Please upload a CSV file.", type = "error")
        return(NULL)
      }

      cropSuitabilityData <- read_csv(input$cropSuitabilityData$datapath)
      cropSuitabilityDataReactive(cropSuitabilityData)
    })

    # Render the crop suitability parameters data table
    output$cropSuitabilityTable <- renderDT({
      datatable(cropSuitabilityDataReactive(), editable = input$editableTable)
    })

    # Observe changes in the editable table and update the data
    observeEvent(input$cropSuitabilityTable_cell_edit, {
      info <- input$cropSuitabilityTable_cell_edit
      cropSuitabilityData <- cropSuitabilityDataReactive()
      cropSuitabilityData[info$row, info$col] <- info$value
      cropSuitabilityDataReactive(cropSuitabilityData)
    })

    # Return the reactive crop suitability data when the "Submit Crop Parameters" button is clicked
    observeEvent(input$submitCropParams, {
      submittedData$cropParams <- cropSuitabilityDataReactive()
      submittedData$cropName <- input$cropName
      showNotification("Crop Parameters submitted successfully!", type = "message")
    })
  })
}

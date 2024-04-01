#' Intervention Lookup Module UI
#'
#' This module provides a user interface for uploading and managing intervention lookup tables.
#'
#' @param id The ID of the module, used to create namespaces for the UI elements.
#'
#' @return A tagList containing the UI elements for the Intervention Lookup module.
#'
#' @export
interventionLookupUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(NS(id, "interventionData"), "Upload Intervention Lookup Table (CSV)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        checkboxInput(NS(id, "editableTable"), "Editable Table", value = FALSE),
        actionButton(NS(id, "submitInterventionLookup"), "Submit Intervention Lookup")
      ),
      mainPanel(
        DTOutput(NS(id, "interventionTable"))
      )
    )
  )
}

#' Intervention Lookup Module Server
#'
#' This module server handles the logic for uploading and managing intervention lookup tables.
#'
#' @param id The ID of the module, used to create namespaces for the server elements.
#' @param submittedData reactive values to store submitted data
#'
#' @return The server logic for the Intervention Lookup module.
#'
#' @importFrom shiny moduleServer reactive observeEvent reactiveVal
#' @importFrom tools file_ext
#' @importFrom DT renderDT
#' @importFrom readr read_csv
#'
#' @export
interventionLookupServer <- function(id, submittedData) {
  moduleServer(id, function(input, output, session) {
    # Create a reactive value to store the intervention lookup data
    interventionDataReactive <- reactiveVal(NULL)

    # Observe changes in the uploaded intervention lookup table CSV file
    observeEvent(input$interventionData, {
      # Validate and read the uploaded file
      ext <- file_ext(input$interventionData$name)
      if (ext != "csv") {
        showNotification("Please upload a CSV file.", type = "error")
        return(NULL)
      }

      interventionData <- read_csv(input$interventionData$datapath)
      interventionDataReactive(interventionData)
    })

    # Render the intervention lookup table
    output$interventionTable <- renderDT({
      datatable(interventionDataReactive(), editable = input$editableTable)
    })

    # Observe changes in the editable table and update the data
    observeEvent(input$interventionTable_cell_edit, {
      info <- input$interventionTable_cell_edit
      interventionData <- interventionDataReactive()
      interventionData[info$row, info$col] <- info$value
      interventionDataReactive(interventionData)
    })

    # Return the reactive intervention lookup data when the "Submit Intervention Lookup" button is clicked
    observeEvent(input$submitInterventionLookup, {
      submittedData$interventionLookup <- interventionDataReactive()
      print(submittedData$interventionLookup)
      showNotification("Intervention Lookup submitted successfully!", type = "message")
    })
  })
}

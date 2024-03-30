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
#'
#' @return The server logic for the Intervention Lookup module.
#'
#' @importFrom shiny moduleServer reactive observeEvent
#' @importFrom tools file_ext
#' @importFrom DT renderDT
#' @importFrom readr read_csv
#'
#' @export
interventionLookupServer <- function(id, submittedData) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression for the uploaded intervention lookup table CSV file
    interventionData <- reactive({
      req(input$interventionData)

      # Validate and read the uploaded file
      ext <- tools::file_ext(input$interventionData$name)
      if (ext != "csv") {
        showNotification("Please upload a CSV file.", type = "error")
        return(NULL)
      }

      readr::read_csv(input$interventionData$datapath)
    })

    # Render the intervention lookup table
    output$interventionTable <- renderDT({
      req(interventionData())
      datatable(interventionData(), editable = input$editableTable)
    })

    # Observe changes in the editable table and update the data
    observeEvent(input$interventionTable_cell_edit, {
      info <- input$interventionTable_cell_edit
      interventionData()[info$row, info$col] <<- info$value
    })

    # Observe submit button clicks in module servers
    observeEvent(input$submitInterventionLookup, {
      submittedData$interventionLookup <- interventionData()
    })

  })
}

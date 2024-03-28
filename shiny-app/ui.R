library(shiny)

ui <- fluidPage(
  titlePanel("Agricultural Land Suitability Analysis"),

  sidebarLayout(
    sidebarPanel(
      fileInput("csvFile", "Upload CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      checkboxInput("editableTable", "Editable Table", value = FALSE),
      actionButton("visualizeRasters", "Visualize Rasters")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("dataTable")),
        tabPanel("Raster Visualization", plotOutput("rasterPlot"))
      )
    )
  )
)

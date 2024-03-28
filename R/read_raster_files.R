#' Read Raster Files
#'
#' This function reads raster files from the provided file paths and returns a list of raster objects.
#' If a file is not found, it attempts to load the raster from the 'ALSA' package.
#'
#' @param climate_soil_data A tibble or data frame containing the paths to raster files (`raster_path`).
#'
#' @return A list of SpatRaster objects representing the loaded raster files.
#' @importFrom terra rast
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming climate_soil_data is predefined
#' raster_objects <- read_raster_files(climate_soil_data)
#' }
read_raster_files <- function(climate_soil_data) {
  # Check if climate_soil_data is a data frame and has the 'raster_path' column
  if (!is.data.frame(climate_soil_data) || !("raster_path" %in% names(climate_soil_data))) {
    stop("climate_soil_data must be a data frame with a 'raster_path' column")
  }

  # Load raster objects from file paths
  climate_soil_data <- climate_soil_data %>%
    mutate(raster_object = map(raster_path, ~ {
      if (file.exists(.x)) {
        rast(.x)
      } else {
        message(paste("File not found:", .x, "Trying to load from package 'ALSA'"))
        rast(system.file(.x, package = "ALSA"))
      }
    }))

  # Return the list of raster objects
  return(pull(climate_soil_data, raster_object))
}

#' Harmonize Raster Layers
#'
#' This function harmonizes a set of raster layers to match the projection, resolution, and extent of a reference raster layer.
#' It is useful for preparing multiple raster datasets for combined spatial analysis.
#'
#' @param climate_soil_data A tibble or data frame containing the paths to raster files (`raster_path`) and their corresponding parameter names (`parameter_name`).
#' @param reference_map A SpatRaster object that serves as the reference for harmonization in terms of projection, resolution, and extent. If not provided, the rasters will not be harmonized.
#'
#' @return A list of SpatRaster objects representing the harmonized raster layers, or the original raster layers if no reference_map is provided.
#' @importFrom terra rast project resample
#' @importFrom dplyr mutate pull
#' @importFrom purrr map
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming climate_soil_data and reference_map are predefined
#' harmonised_rasters <- harmonize_rasters(climate_soil_data, reference_map)
#' }
harmonize_rasters <- function(climate_soil_data, reference_map = NULL) {
  # Check if climate_soil_data is a data frame and has the required columns
  if (!is.data.frame(climate_soil_data) || !all(c("parameter_name", "availability", "raster_path") %in% names(climate_soil_data))) {
    stop("climate_soil_data must be a data frame with columns 'parameter_name', 'availability', and 'raster_path'")
  }

  # Load raster objects from file paths using read_raster_files function
  raster_objects <- read_raster_files(climate_soil_data)

  # Combine raster objects with parameter names
  climate_soil_data <- climate_soil_data %>%
    mutate(raster_object = raster_objects)

  # Harmonize raster layers if reference_map is provided
  if (!is.null(reference_map) && "SpatRaster" %in% class(reference_map)) {
    climate_soil_data <- climate_soil_data %>%
      mutate(
        raster_object = map(raster_object, ~ terra::project(.x, reference_map)),
        raster_object = map(raster_object, ~ resample(.x, reference_map))
      )
  } else {
    warning("reference_map is not provided or is not a SpatRaster object. Skipping harmonization.")
  }

  # Return the list of harmonized raster objects
  return(pull(climate_soil_data, raster_object))
}

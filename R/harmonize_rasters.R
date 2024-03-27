#' Harmonize Raster Layers
#'
#' This function harmonizes a set of raster layers to match the projection, resolution, and extent of a reference raster layer.
#' It is useful for preparing multiple raster datasets for combined spatial analysis.
#'
#' @param climate_soil_data A tibble or data frame containing the paths to raster files (`raster_path`) and their corresponding parameter names (`parameter_name`).
#' @param reference_map A SpatRaster object that serves as the reference for harmonization in terms of projection, resolution, and extent.
#'
#' @return A SpatRaster object representing the harmonized raster layers.
#' @importFrom terra rast project resample
#' @importFrom dplyr mutate pull
#' @importFrom purrr map map2
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming climate_soil_data and reference_map are predefined
#'   harmonised_rasters <- harmonize_rasters(climate_soil_data, reference_map)
#' }
harmonize_rasters <- function(climate_soil_data, reference_map) {
  # Check if climate_soil_data is a tibble or dataframe and has required columns
  if (!is.data.frame(climate_soil_data) ||
      !all(c("parameter_name", "availability", "raster_path") %in% names(climate_soil_data))) {
    stop("climate_soil_data must be a tibble or dataframe containing parameter_name, availability, and raster_path columns")
  }

  # Check if reference_map is a SpatRaster object
  if (!("SpatRaster" %in% class(reference_map))) {
    stop("reference_map must be a SpatRaster object")
  }

  climate_soil_data |>
    # Load raster layers
    mutate(raster_object = map(raster_path, rast)) |>
    # Assign names to each raster layer
    mutate(raster_object = map2(raster_object, parameter_name, setNames)) |>
    # Harmonize all rasters to have the same projection
    mutate(raster_object = map(raster_object, ~ terra::project(.x, reference_map))) |>
    # Harmonize all rasters to have the same resolution and extent as the reference raster
    mutate(raster_object = map(raster_object, ~ resample(.x, reference_map))) |>
    pull(raster_object) |> rast()
}

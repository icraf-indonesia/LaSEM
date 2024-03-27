#' Harmonize Raster Layers
#'
#' This function harmonizes a set of raster layers to match the projection, resolution, and extent of a reference raster layer.
#' It is useful for preparing multiple raster datasets for combined spatial analysis.
#'
#' @param climate_soil_data A tibble or data frame containing the paths to raster files (`raster_path`) and their corresponding parameter names (`parameter_name`).
#' @param reference_map A SpatRaster object that serves as the reference for harmonization in terms of projection, resolution, and extent. If not provided, the rasters will not be harmonized.
#'
#' @return A SpatRaster object representing the harmonized raster layers, or the original raster layers if no reference_map is provided.
#' @importFrom terra rast project resample names varnames
#' @importFrom dplyr mutate pull
#' @importFrom purrr map map2
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming climate_soil_data and reference_map are predefined
#'   harmonised_rasters <- harmonize_rasters(climate_soil_data, reference_map)
#' }
harmonize_rasters <-
  function(climate_soil_data, reference_map = NULL) {
    stopifnot(is.data.frame(climate_soil_data),
              all(
                c("parameter_name", "availability", "raster_path") %in% names(climate_soil_data)
              ))

    climate_soil_data <- climate_soil_data %>%
      mutate(raster_object = map(raster_path, ~ if (file.exists(.x))
        rast(.x)
        else
          rast(system.file(.x, package = "ALSA"))))

    if (!is.null(reference_map) &&
        "SpatRaster" %in% class(reference_map)) {
      climate_soil_data <- climate_soil_data %>%
        mutate(
          raster_object = map(raster_object, ~ terra::project(.x, reference_map)),
          raster_object = map(raster_object, ~ resample(.x, reference_map))
        )
    }

    stacked_raster_factors <- climate_soil_data %>%
      mutate(raster_object = map2(raster_object, parameter_name, setNames)) %>%
      pull(raster_object) %>%
      rast()

    varnames(stacked_raster_factors) <- names(stacked_raster_factors)

    return(stacked_raster_factors)
}

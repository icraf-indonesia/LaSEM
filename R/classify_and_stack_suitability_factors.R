#' Classify and Stack Suitability Factors
#'
#' This function processes each layer of a stacked `SpatRaster` object
#' using the `classify_suitability_predictors` function and returns a stacked
#' raster with classified layers.
#'
#' @param stacked_raster A stacked `SpatRaster` object.
#' @param suitability_data A data frame containing crop suitability parameters.
#' @return A stacked `SpatRaster` object with classified layers.
#'
#' @examples
#' \dontrun{
#'   # Assuming you have suitability_factors
#'   # as a SpatRaster and crop_suitability as a data frame
#'   stacked_suitability <- classify_and_stack_suitability_factors(
#'                               suitability_factors, crop_suitability)
#' }
#' @importFrom terra rast nlyr
#' @export
classify_and_stack_suitability_factors <- function(stacked_raster, suitability_data) {
  # Apply the classify_suitability_predictors function to each layer using lapply

  reclassified_rasters <- lapply(1:nlyr(stacked_raster), function(i) {
    #print(names(stacked_raster[[i]]))
    classify_suitability_predictors(stacked_raster[[i]], suitability_data)
  })

  # Stack the reclassified rasters
  stacked_suitability <- rast(reclassified_rasters)

  # Return the stacked raster
  return(stacked_suitability)
}

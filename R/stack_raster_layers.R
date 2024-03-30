#' Stack Raster Layers
#'
#' This function stacks a list of raster layers and sets the variable names based on the corresponding parameter names.
#'
#' @param raster_list A list of SpatRaster objects to be stacked.
#' @param parameter_names A vector of parameter names corresponding to each raster layer in the list.
#'
#' @return A SpatRaster object representing the stacked raster layers with variable names set.
#' @importFrom terra rast names varnames
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming raster_list and parameter_names are predefined
#' stacked_rasters <- stack_raster_layers(raster_list, parameter_names)
#' }
stack_raster_layers <- function(raster_list, parameter_names) {
  # Check if raster_list is a list and parameter_names is a vector of the same length
  if (!is.list(raster_list) || !is.vector(parameter_names) || length(raster_list) != length(parameter_names)) {
    stop("raster_list must be a list and parameter_names must be a vector of the same length as raster_list")
  }

  # Set names of raster objects based on parameter names
  named_raster_list <- setNames(raster_list, parameter_names)

  # Stack the raster layers
  stacked_raster_factors <- rast(named_raster_list)

  # Set variable names of the stacked raster object
  #varnames(stacked_raster_factors) <- names(stacked_raster_factors)

  return(stacked_raster_factors)
}

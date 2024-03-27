#' Create Layer Dataframe from SpatRaster
#'
#' This function takes a `SpatRaster` object and returns a dataframe with two columns:
#' `ID` and `names`. `ID` is a sequence number starting from 1 for each layer in the
#' SpatRaster, and `names` are the names of these layers.
#'
#' @param spatraster A `SpatRaster` object.
#'
#' @return A dataframe with two columns: `ID` and `names`.
#'         `ID` is a sequence from 1 to the number of layers in the `SpatRaster` object,
#'         and `names` are the names of the layers in the `SpatRaster` object.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a SpatRaster object named suitability_factors
#' # suitability_factors <- ...
#' dataframe <- create_layer_dataframe(suitability_factors)
#' print(dataframe)
#' }
#'
#' @export
create_layer_dataframe <- function(spatraster) {
  # Extract the names of the layers
  layer_names <- names(spatraster)

  # Create a sequence of IDs
  ids <- seq_along(layer_names)

  # Combine into a dataframe
  dataframe <- data.frame(ID = ids, names = layer_names)

  return(dataframe)
}

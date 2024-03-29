#' Concatenate Raster Layers
#'
#' This function concatenates all raster layers within a `terra` raster object
#' into a single raster object.
#'
#' @param rasters A `terra` raster object containing multiple raster layers.
#'
#' @return A single `terra` raster object containing all concatenated layers.
#'
#' @importFrom terra rast nlyr concats droplevels
#'
#' @export
#'
#' @examples
#' # Example usage
#' rasters <- terra::rast(system.file("ex/rast.tif", package = "terra"))
#' concatenated_raster <- concat_rasters(rasdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==ters)
concat_rasters <- function(rasters) {
  result <- rasters[[1]]
  for (i in 2:nlyr(rasters)) {
    result <- concats(result, rasters[[i]]) |>
      droplevels()
  }
  return(result)
}

concat_rasters <- function(rasters) {
  result <- rasters[[1]]
  for(i in 2:terra::nlyr(rasters)) {
    result <- terra::concats(result, rasters[[i]]) |>
      terra::droplevels()
  }
  return(result)
}

#' Print Information about Analyzed Names
#'
#' This function prints the number of objects and their names
#' contained in the input vector. It's useful for quickly viewing
#' the contents and the count of elements in the vector.
#'
#' @param names_analysed A vector of names (character) to be analyzed.
#'
#' @return None. This function is used for its side effect of printing
#' to the console.
#'
#' @examples
#' \dontrun{
#' names_analysed <- c("clim_temperature_avg", "clim_precipitation_tot",
#'                     "clim_humidity", "soil_coarse_fragments", "soil_depth",
#'                     "soil_cec", "soil_base_saturation", "soil_ph",
#'                     "soil_organic_c", "soil_nitrogen", "soil_salinity",
#'                     "soil_esp", "soil_slope")
#' print_names_analysed_info(names_analysed)
#' }
#'
print_names_analysed_info <- function(names_analysed) {
  # Find the number of names
  num_names <- length(names_analysed)

  # Create the message
  message <- paste("Number of objects:", num_names, "\nNames of objects:", paste(names_analysed, collapse = ", "))

  # Print the message
  print(message)
}

#' Determine Suitability and Limiting Factors
#'
#' This function evaluates a vector of suitability classes and determines the
#' lowest suitability class based on a predefined priority order. It also
#' identifies the positions of this class within the vector.
#'
#' @param class_vector A vector of characters representing suitability classes.
#'        The classes are expected to be "S1", "S2", "S3", or "N", with "N" being
#'        the lowest and "S1" the highest suitability class.
#'
#' @return A list containing two elements: 'suitability', which is the lowest
#'         suitability class found in the input vector, and 'limiting_factor',
#'         which is a vector of positions where this class occurs in the input.
#'         If the highest class is "S1", 'limiting_factor' is set to NA.
#'
#' @examples
#' determine_suitability(c("S1", "S1", "S3", "S3", "S2"))
#'
#' @export
determine_suitability <- function(class_vector) {
  # Define the order of priority for the suitability classes
  priority_order <- c("N", "S3", "S2", "S1")

  # Determine the highest priority class present in the vector
  lowest_class <- priority_order |>
    purrr::map_chr(~ifelse(any(class_vector == .x), .x, NA_character_)) |>
    na.omit() |>
    (\(.) .[1])()

  # Determine the positions of the highest priority class
  limiting_factor <- which(class_vector == lowest_class)

  # If the highest class is "S1", set limiting_factor to NA
  if (lowest_class == "S1") {
    limiting_factor <- NULL
  }

  # Return a list containing suitability and limiting factors
  return(list(suitability = lowest_class, limiting_factor = limiting_factor))
}

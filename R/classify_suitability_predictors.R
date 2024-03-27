#' Classify raster based on crop suitability parameters
#'
#' @param raster_input A SpatRaster object.
#' @param suitability_data A data frame containing crop suitability parameters.
#' @return A reclassified SpatRaster.
#' @examples
#' \dontrun{
#' classified_raster <- classify_suitability_predictors(
#'                       raster_input = clim_temperature_avg,
#'                       suitability_data = crop_suitability)
#' }
#' @export
classify_suitability_predictors <- function(raster_input, suitability_data) {
  # Check if raster_input is of the correct type
  if (!inherits(raster_input, "SpatRaster")) {
    stop("raster_input must be a SpatRaster object")
  }

  # Check if suitability_data is of the correct type
  if (!is.data.frame(suitability_data)) {
    stop("suitability_data must be a data.frame")
  }

  # Check if the necessary columns exist in suitability_data
  required_columns <- c("value", "class", "name_parameter")
  if (!all(required_columns %in% names(suitability_data))) {
    stop(paste("suitability_data must contain the following columns:", paste(required_columns, collapse = ", ")))
  }

  # Check if the name of raster_input exists in the name_parameter column of suitability_data
  if (!(names(raster_input) %in% suitability_data$name_parameter)) {
    stop(paste("The name of raster_input must exist in the name_parameter column of suitability_data. Please check your inputs."))
  }
  # Filter suitability_data to match raster_input
  suitability_data <- suitability_data %>%
    filter(name_parameter %in% names(raster_input))

  if(suitability_data[["name_parameter"]][1] == "soil_texture"){
    texture_lookup <- read_csv("data/lookup_tables/lookup_texture_usda.csv") |> select(texture_kemtan, TEXTURE_USDA )

    # Apply the mapping function to each element in the value list
    suitability_data <- suitability_data %>%
      mutate(value = map(value, ~ str_split(.x, pattern = "_", simplify = TRUE))) %>%
      mutate(value = map(value, ~ map(.x, map_texture_code, lookup_table = texture_lookup))) |>
      mutate(value = map(value, unlist)) |>
      mutate(class = factor(class, levels = c("S1", "S2", "S3", "N")))

    reclass_matrix <- suitability_data |>
      select(value, class) |>
      unnest_longer(value) |>
      mutate(class = as.numeric(class)) |>
      as.matrix()

  } else {

    # Pre-process the suitability data frame
    suppressWarnings({
      suitability_data <- suitability_data %>%
        mutate(
          lower = case_when(
            str_starts(value, ">") ~ as.numeric(str_extract(value, "\\d+\\.?\\d*$")),
            str_detect(value, "\\d+-\\d+") ~ as.numeric(str_extract(value, "^\\d+\\.?\\d*")),
            TRUE ~ NA_real_
          ),
          upper = ifelse(str_ends(value, "<"), as.numeric(str_extract(
            value, "\\d+\\.?\\d*"
          )), NA)
        ) %>%
        mutate(
          upper = ifelse(is.na(upper), ifelse(
            str_starts(value, ">"), Inf, as.numeric(str_extract(value, "\\d+\\.?\\d*$"))
          ), upper),
          lower = replace_na(lower,-Inf),
          class = factor(class, levels = c("S1", "S2", "S3", "N"))
        )

    })



    # Define the reclassification matrix
    reclass_matrix <- suitability_data %>%
      dplyr::select(lower, upper, class) %>%
      mutate(class = as.numeric(class)) %>%
      as.matrix()
  }

  rast_name <- names(raster_input)

  # Reclassify the raster
  r_reclassified <- terra::classify(raster_input, rcl = reclass_matrix,
                                    include.lowest = TRUE)

  # Create the lookup data frame
  lookup_df <- data.frame(
    class = 1:4,
    level = c("S1", "S2", "S3", "N")
  )

  # Reassign raster values to predefined levels
  levels(r_reclassified)<- lookup_df
  names(r_reclassified) <- rast_name
  r_reclassified<- terra::droplevels(r_reclassified)

  return(r_reclassified)
}

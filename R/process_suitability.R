#' Process Suitability Factors for Crop Suitability Analysis
#'
#' This function integrates environmental suitability factors with crop suitability parameters to produce
#' a comprehensive analysis of crop suitability. It involves reclassifying raster layers based on
#' suitability parameters, creating a frequency table, and converting rasters to spatial polygons.
#' This function relies on specific functions from terra, dplyr, tidyr, and sf packages.
#'
#' @param suitability_factors A SpatRaster object from the terra package representing environmental factors
#' affecting crop growth such as soil quality, climate conditions, etc.
#' @param crop_suitability A dataframe with crop suitability parameters, each row representing a different
#' parameter and its associated suitability conditions.
#'
#' @return A list containing four elements:
#' \itemize{
#'   \item{suitability_raster}{A SpatRaster object representing the combined suitability analysis.}
#'   \item{suitability_polygon}{An sf object representing suitability areas as polygons.}
#'   \item{suitability_attr}{A dataframe containing attributes for each suitability category.}
#'   \item{suitability_by_factors}{A list of SpatRaster objects for individual suitability factors.}
#' }
#' @importFrom terra freq cats as.polygons subset activeCat levels
#' @importFrom dplyr select left_join mutate group_by summarise pull filter row_number rowwise rename pick
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom sf st_as_sf
#' @importFrom purrr map map2
#' @importFrom rlang .data
#' @importFrom tibble as_tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming suitability_factors and crop_suitability are already defined
#'   suitability_results <- process_suitability(suitability_factors, crop_suitability)
#'   View(suitability_results$suitability_raster)
#'   View(suitability_results$suitability_polygon)
#'   View(suitability_results$suitability_attr)
#'   # Explore individual factor suitability rasters
#'   lapply(suitability_results$suitability_by_factors, View)
#' }
# Function to Process Suitability Data for Crop Suitability Analysis
process_suitability <- function(suitability_factors, crop_suitability) {

  # Step 1: Create a DataFrame for Layer Lookup
  # This step involves creating a DataFrame that maps each layer in the suitability_factors to a name.
  lookup_suitability_factors <- create_layer_dataframe(suitability_factors)
  lookup_suitability_factors_names <- lookup_suitability_factors |> pull(names)

  # Step 2: Identify and List Crop Parameters
  # Extracts unique parameter names from the crop_suitability DataFrame.
  crop_suitability_param_names <- crop_suitability |> pull(name_parameter) |> unique()

  # Step 3: Identify Names Not in Crop Suitability
  # Finds layer names in lookup_suitability_factors that are not in crop_suitability parameters.
  names_not_in_crop_suitability <- setdiff(lookup_suitability_factors_names, crop_suitability_param_names)

  # Step 4: Subset Suitability Factors
  # If there are names not in crop suitability, subset the suitability factors to exclude them.
  if (!is.null(names_not_in_crop_suitability)) {
    suitability_factors <- terra::subset(suitability_factors, names_not_in_crop_suitability, negate = TRUE)
    lookup_suitability_factors <- lookup_suitability_factors |>
      filter(!names %in% names_not_in_crop_suitability) |>
      dplyr::mutate(ID = row_number())
  }

  # Step 5: Cross-Check and Print Names Analysed
  # Intersects the names of the suitability factors with the crop suitability parameters and prints them.
  names_analysed <- intersect(names(suitability_factors), crop_suitability_param_names)
  print_names_analysed_info(names_analysed)

  # Step 6: Classify Suitability of Each Predictor
  # Applies predefined functions to classify and stack suitability factors.
  suitability_factors_reclass <- classify_and_stack_suitability_factors(
    stacked_raster = suitability_factors,
    suitability_data = crop_suitability)

  # Step 7: Combine Suitability Rasters
  # Concatenates the levels of all the suitability rasters into one.
  suitability_raster <- concat_rasters(suitability_factors_reclass)

  # Step 8: Create Frequency Table for Suitability Raster
  # Generates a frequency table for the combined suitability raster.
  suitability_raster_freq <- terra::freq(suitability_raster) |> dplyr::select(categories = value, count)
  # Step 9: Extract and Process Attribute Table
  # Processes the attribute table of the suitability raster for further analysis.
  suitability_attr <- terra::levels(suitability_raster)[[1]] |>
    as_tibble() |>
    rename(categories = 2) |>
    left_join(suitability_raster_freq, by = "categories") |>
    mutate(class_category = strsplit(as.character(categories), "_")) |>
    rowwise() |>
    mutate(class = list(determine_suitability(class_category))) |>
    tidyr::unnest_wider(class) |>
    mutate(limiting_factor_id = limiting_factor) |>
    tidyr::unnest_longer(limiting_factor_id, keep_empty = TRUE) |>
    left_join(lookup_suitability_factors, by = c("limiting_factor_id" = "ID")) |>
    rename(limiting_factor_actual = names) |>
    group_by(ID, categories, class_category, suitability, count) |>
    summarise(limiting_factor_actual = list(pick(limiting_factor_actual)), .groups = 'drop') |>
    unnest_longer(col = class_category) |>
    group_by(ID) |>
    mutate(id_factor = seq_along(class_category)) |>
    left_join(lookup_suitability_factors, by = c("id_factor" = "ID")) |>
    mutate(names = ifelse(class_category %in% "S1", NA, names)) |>
    group_by(ID, categories, suitability, count, limiting_factor_actual) |>
    rename(limiting_factor_potential = names) |>
    summarise(limiting_factor_potential = list(na.omit(pick(limiting_factor_potential))), .groups = 'drop') |>
    mutate(limiting_factor_potential = map2(limiting_factor_actual, limiting_factor_potential, ~ {
      potential_unique <- setdiff(.y$limiting_factor_potential, .x$limiting_factor_actual)

      if (length(potential_unique) == 0) {
        return(NA)
      } else {
        return(tibble(limiting_factor_potential = potential_unique))
      }
    })) |>
    mutate(limiting_factor_actual = map(limiting_factor_actual, ~unlist(.x, use.names=FALSE))) |>
    mutate(limiting_factor_potential = map(limiting_factor_potential, ~unlist(.x, use.names=FALSE)))


  # Step 10: Update Levels of the Suitability Raster
  # Updates the categorical levels of the suitability raster based on the attribute table.
  levels(suitability_raster) <- as.data.frame(suitability_attr)
  terra::activeCat(suitability_raster) <- "ID"

  # Step 11: Convert Raster to Polygons and Join with Attribute Table
  # Converts the raster data into polygon format and merges it with the attribute table.
  suitability_polygon <- suitability_raster |>
    as.polygons() |>
    sf::st_as_sf() |>
    left_join(suitability_attr, by = "ID")

  # Step 12: Return Results
  # Returns a list containing the processed data in various formats.
  return(list(suitability_raster = suitability_raster,
              suitability_polygon = suitability_polygon,
              suitability_attr = suitability_attr,
              suitability_by_factors = suitability_factors_reclass,
              lookup_suitability_factors = lookup_suitability_factors))
}

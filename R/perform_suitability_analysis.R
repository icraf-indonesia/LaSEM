#' Perform Suitability Analysis
#'
#' This function performs suitability analysis based on harmonised rasters and suitability parameters.
#' It calculates the actual and potential suitability maps and returns the results as a list.
#'
#' @param harmonised_rasters A SpatRaster object representing the harmonised raster layers.
#' @param suitability_parameter A data frame or tibble containing the crop suitability parameters.
#' @param lookup_intervention A tibble of intervention options
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{suitability_map}{A list with the actual suitability map and related data.}
#'   \item{suitability_polygon}{A polygon layer with potential suitability attributes.}
#' }
#'
#' @importFrom terra rast
#' @importFrom dplyr left_join mutate case_when
#' @importFrom sf st_transform
#' @importFrom purrr map
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming harmonised_rasters, suitability_parameter,
#'   # and path_lookup_intervention are predefined
#'   result <- perform_suitability_analysis(harmonised_rasters,
#'   suitability_parameter, path_lookup_intervention)
#' }
perform_suitability_analysis <-
  function(harmonised_rasters,
           suitability_parameter,
           lookup_intervention) {


    # Check input classes
    stopifnot(
      inherits(harmonised_rasters, "SpatRaster"),
      is.data.frame(suitability_parameter) ||
        inherits(suitability_parameter, "tbl_df")
    )

    # 3a. Suitability Analysis Actual
    suitability_map <-
      process_suitability(suitability_factors = harmonised_rasters,
                          crop_suitability = suitability_parameter)

    # 3b. Suitability Analysis Potential
    intervention_table <- list("low", "med", "high") %>%
      map(
        ~ calculate_suitability_potential_table(
          lookup_intervention = lookup_intervention,
          intervention_level = .,
          suitability_attr = suitability_map[["suitability_attr"]],
          lookup_suitability_layer = suitability_map[["lookup_suitability_factors"]]
        )
      )

    sutability_attr_pot <-
      Reduce(function(x, y)
        left_join(x, y, by = "ID"), intervention_table)

    suitability_polygon <- suitability_map$suitability_polygon |>
      left_join(sutability_attr_pot, by = "ID") |>
      mutate(
        suitability_potential_low  = case_when(suitability == "S1" ~ "S1", .default = suitability_potential_low),
        suitability_potential_med  = case_when(suitability == "S1" ~ "S1", .default = suitability_potential_med),
        suitability_potential_high  = case_when(suitability == "S1" ~ "S1", .default = suitability_potential_high)
      ) |>
      st_transform(crs = 4326)

    suitability_map$suitability_polygon <- suitability_polygon

    return(suitability_map)
  }

#' Calculate Suitability Potential Table
#'
#' This function reads a CSV file for lookup intervention data and processes it along with a given suitability attribute dataframe to calculate the suitability potential table.
#'
#' @param path_lookup_intervention A string path to the CSV file containing lookup intervention data.
#' @param intervention_level A string indicating the level of intervention.
#' @param suitability_attr A dataframe containing suitability attributes with columns name_parameter, ID, intervention, limiting_factor_actual, and suitability.
#' @param lookup_suitability_layer A dataframe with two columns: `ID` and `names`.
#'         `ID` is a sequence from 1 to the number of layers in the `SpatRaster` object,
#'         and `names` are the names of the layers in the `SpatRaster` object.
#'
#' @return A tibble containing the calculated suitability potential table.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr select mutate group_by ungroup summarise rename_with
#' @importFrom tidyr unnest_longer
#' @importFrom stringr str_count
#'
#' @examples
#' \dontrun{
#'   path <- "data/lookup_tables/lookup_intervention.csv"
#'   level <- "high"
#'   attr <- readr::read_csv("output/kesesuaian_jagung_aktual.csv")
#'   calculate_suitability_potential_table(path, level, attr)
#' }
calculate_suitability_potential_table <- function(path_lookup_intervention, intervention_level, suitability_attr, lookup_suitability_layer) {

  # Check for input types and specifications
  if (!is.character(path_lookup_intervention) || length(path_lookup_intervention) != 1) {
    stop("path_lookup_intervention must be a single object string.")
  }

  if (!file.exists(path_lookup_intervention)) {
    stop("File specified in path_lookup_intervention does not exist.")
  }

  if (!is.character(intervention_level) || length(intervention_level) != 1) {
    stop("intervention_level must be a single object string.")
  }

  if (!is_tibble(suitability_attr) ||
      !all(c( "ID",  "limiting_factor_actual", "suitability") %in% names(suitability_attr))) {
    stop("suitability_attr must be a data frame with specific columns.")
  }

  # Reading and processing the lookup intervention data
  lookup_intervention <- read_csv(path_lookup_intervention) |>
    select(-c("no", "karakteristik_lahan")) |>
    mutate(
      low = ifelse(is.na(low), 0, str_count(low, pattern = fixed("+"))),
      med = ifelse(is.na(med), 0, str_count(med, pattern = fixed("+"))),
      high = ifelse(is.na(high), 0, str_count(high, pattern = fixed("+")))
    ) |>
    group_by(name_parameter, intervention)

  # Filtering and renaming
  lookup_intervention_filtered <- lookup_intervention |>
    dplyr::select(all_of(intervention_level)) |>
    ungroup() |>
    rename(dummy_intervention = intervention_level)
  # Creating the suitability potential table
  # Add a check if maximum is higher or equal with the list of uncontrolled limiting factor then update limiting_factor_name, also update the intervention_potential
  suitability_potential_table <- suitability_attr |>
    mutate(class_category = strsplit(as.character(categories), "_")) |>
    dplyr::select(ID, suitability, class_category ) |> # take it from class_category instead, do not use limiting factor name, then unnest longer class_category, join with the lookup_suitability_factors, the rest are fine
    #mutate(limiting_factor_name = str_split(limiting_factor_name, ", ")) |>
    unnest_longer(col = class_category) |>
    group_by(ID) |>
    mutate(id_factor = seq_along(class_category)) |>
    left_join(lookup_suitability_layer, by = c("id_factor" = "ID")) |>
    left_join(lookup_intervention_filtered, by = c("names" = "name_parameter")) |>
    mutate(
      dummy_suitability = case_when(
        class_category == "S1" ~ 1,
        class_category == "S2" ~ 2,
        class_category == "S3" ~ 3,
        class_category == "N"  ~ 4,
        TRUE ~ NA_real_
      )
    ) |>
    filter(!suitability %in% "S1") |>
    filter(!class_category  %in% "S1") |>
    mutate(
      dummy_suitability_pot_id = dummy_suitability - dummy_intervention,
      dummy_suitability_pot_id = case_when(dummy_suitability_pot_id < 1 ~ 1, TRUE ~ dummy_suitability_pot_id)
    ) |>
    group_by(ID, suitability) |>
    summarise(
      dummy_suitability_potential = max(dummy_suitability_pot_id, na.rm = FALSE),
      #limiting_factors_complete =  list(pick(names)),
      # intervention_potential = case_when(
      #   all(intervention == TRUE) ~ "all",
      #   any(intervention == TRUE) & any(intervention == FALSE) ~ "partial",
      #   !any(intervention == TRUE) ~ "none"
      # ),
      .groups = "drop") |>
    mutate(
      suitability_potential = paste0("S", dummy_suitability_potential),
      suitability_potential = case_when(suitability_potential %in% "S4" ~ "N", TRUE ~ suitability_potential),
      suitability_potential = case_when(suitability_potential %in% "SNA" ~ suitability, TRUE ~ suitability_potential)
    ) |>
    dplyr::select(-contains("dummy"), -suitability) |>
    rename_with(~ paste(., intervention_level, sep = "_"), -c("ID"))

  return(suitability_potential_table)
}

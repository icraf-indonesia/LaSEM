#' Map Texture Code to TEXTURE_USDA Value
#'
#' This function maps a texture code to its corresponding TEXTURE_USDA value using a lookup table.
#'
#' @param texture_code A character string representing the texture code.
#' @param lookup_table A data frame containing the mapping between texture codes and TEXTURE_USDA values.
#'
#' @return A character string representing the TEXTURE_USDA value corresponding to the input texture code.
#'         If the texture code is not found in the lookup table, the function returns NA.
#'
#' @importFrom dplyr filter pull
#'
#' @examples
#' lookup_table <- data.frame(
#'   texture_kemtan = c("1", "2", "3"),
#'   TEXTURE_USDA = c("Sandy", "Loamy", "Clayey")
#' )
#'
#' map_texture_code("1", lookup_table)  # Returns "Sandy"
#' map_texture_code("4", lookup_table)  # Returns NA
#'
#' @export
map_texture_code <- function(texture_code, lookup_table) {
  texture_usda <- lookup_table %>%
    filter(texture_kemtan == texture_code) %>%
    pull(TEXTURE_USDA)

  if (length(texture_usda) == 0) {
    return(NA)
  }

  return(texture_usda)
}

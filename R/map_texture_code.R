# Define a function to map each texture code to its TEXTURE_USDA value
map_texture_code <- function(texture_code, lookup_table) {
  texture_usda <- lookup_table %>%
    filter(texture_kemtan == texture_code) %>%
    pull(TEXTURE_USDA)

  if (length(texture_usda) == 0) {
    return(NA)
  }

  return(texture_usda)
}

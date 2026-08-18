  #' Normalise the column names of the checklist sheets
  #'
  #' The wamei checklist sheets have column names such as `all name`,
  #' `lato/stricto` and `Family name (JP)`.  Turn the spaces and slashes
  #' into underscores and drop the parentheses, so that the rest of the
  #' package can refer to `all_name`, `lato_stricto` and `Family_name_JP`.
  #'
  #' @param x A data frame read from the wamei checklist.
  #'
  #' @return The same data frame with normalised column names.
  #'
  #' @noRd
clean_colnames <- function(x){
  x %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_remove_all(., "[()]"))
}

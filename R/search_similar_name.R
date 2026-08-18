  #' Search similar name(s) from existing data (deprecated)
  #'
  #' Deprecated in 0.9.3, to be removed in 0.10.0.
  #' Use [maybe()] for scientific names and [mosiya()] for wamei
  #' (Japanese names).  Both take a vector, compute the edit distances in
  #' C++ instead of looping in R, and return the same information under
  #' clearer column names (`reference` `editdist` `editdist_norm`
  #' instead of `maybe` `dist` `dist_norm`).
  #'
  #' @param x A String or a vector of strings to be checked.
  #' @param len integer 1: when checking scientific name, 6: when checking wamei (Japanese name).
  #' @param min_dist Minimum editing distance. Less than min_dist will be output.
  #' @param min_dist_norm Minimum normalised editing distance.
  #'
  #' @return Tibble.
  #'
  #' @seealso maybe(), mosiya()
  #'
  #' @export
search_similar_name <- function(x, len=1, min_dist=4, min_dist_norm=0.2){
  .Deprecated(if(len == 6) "mosiya" else "maybe",
              package = "wameicheckr",
              msg = paste0("'search_similar_name' is deprecated and will be ",
                           "removed in wameicheckr 0.10.0.\n",
                           "Use 'mosiya()' for wamei or 'maybe()' for ",
                           "scientific names instead."))
  # LazyData: true なので data() は不要
  ref <- if(len==6){
    ref_jp   # when wamei (Japanese name)
  } else {
    ref_sc   # when scientific name
  }
  res <-
    ref %>%
    dplyr::select(tidyselect::contains("name")) %>%
    .[[1]] %>%
    purrr::map(editdist, x, len=len) %>%  # editdist: cpp code to speed up
    unlist() %>%
    tibble::tibble(x, ref, dist=.) %>%
    magrittr::set_colnames(c("input", "source", "maybe", "dist"))
  res <-
    if(len==6){  # when wamei (Japanese name), unescape wamei (Japanese name)
      dplyr::mutate(res, input=stringi::stri_unescape_unicode(input), maybe=stringi::stri_unescape_unicode(maybe))
    } else {     # when scientific name
      res
    }
  res <-
    res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dist_norm = dist / max(stringr::str_length(input), stringr::str_length(maybe))) %>%
    dplyr::filter(dist < min_dist | dist_norm < min_dist_norm) %>%
    dplyr::distinct() %>%
    dplyr::mutate(tmp="tmp") %>%
    tidyr::pivot_wider(
      id_cols=c(input, maybe, dist, dist_norm), names_from=tmp, values_from=source,
      values_fn = list(source = ~paste(., collapse = "; "))
    )
  if(nrow(res) > 0) dplyr::rename(res, source=tmp) else res
}

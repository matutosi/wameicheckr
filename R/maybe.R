  #' Search similar names from existing data
  #' 
  #' search_similar_name() for one string. 
  #' maybe() for multiple scientific names.
  #' mosiya() for multiple wamei (Japanese names).
  #' 
  #' @param x A String to be checked. 
  #' @param len integer 1: when checking scientific name, 6: when checking wamei (Japanese name).
  #' @param min_dist Minimum editing distance. Less than min_dist will be output.
  #' @param min_dist_norm Minimum normalised editing distance.
  #' 
  #' @return tibble. 
  #' 
  #' @seealso maybe(), mosiya()
  #' 
  #' @examples
  #' library(stringi)
  #' library(tidyverse)
  #' x <- "Viola madahuricaa"
  #' search_similar_name(x, len=1)
  #' 
  #' x <- c("\\u30cf\\u30c3\\u30ab\\u30b0\\u30b5") %>% 
  #'   stringi::stri_unescape_unicode()
  #' search_similar_name(x, len=6)
  #' 
  #' x <- c("Viola madahuricaa", "Carex nevarta")
  #' maybe(x)
  #' 
  #' x <- 
  #'   c("\\u30cf\\u30c3\\u30ab\\u30b0\\u30b5", 
  #'     "\\u30b9\\u30ba\\u30ce\\u30a8\\u30f3\\u30c9\\u30a6") %>%
  #'   stringi::stri_unescape_unicode()
  #' x
  #' mosiya(x)
  #' 
  #' @export
search_similar_name <- function(x, len=1, min_dist=4, min_dist_norm=0.2){
  ref <- if(len==6){
    data(ref_jp)
    ref_jp   # when wamei (Japanese name)
  } else {
    data(ref_sc)
    ref_sc   # when scientific name
  }
  res <-
    ref %>%
    dplyr::select(tidyselect::contains("name")) %>%
    .[[1]] %>%
    purrr::map(editdist, x, len=len) %>%  # editdist: cpp code to spped up
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
    #   dplyr::mutate(dist_norm = dist / max(stringr::str_length(input), stringr::str_length(maybe)), match=stringr::str_detect(maybe, input)) %>%
    #   dplyr::filter(dist < 3 | dist_norm < 0.2 | (match==TRUE & str_length(input) > 4))
    dplyr::distinct() %>%
    dplyr::mutate(tmp="tmp") %>%
    tidyr::pivot_wider(
      id_cols=c(input, maybe, dist, dist_norm), names_from=tmp, values_from=source, 
      values_fn = list(source = ~paste(., collapse = "; "))
    )
  if(nrow(res) > 0) dplyr::rename(res, source=tmp) else res
}

  #' @rdname search_similar_name
  #' @export
maybe <- function(x, len=1, min_dist=4, min_dist_norm=0.2){
  x %>%
    purrr::map(search_similar_name, len, min_dist, min_dist_norm) %>%
    dplyr::bind_rows()
}

  #' @rdname search_similar_name
  #' @export
mosiya <- function(x, len=6, min_dist=3, min_dist_norm=0.2){
  x <- 
    x %>% 
    stringi::stri_trans_general("halfwidth-fullwidth") %>%
    stringi::stri_escape_unicode()
  maybe(x, len, min_dist, min_dist_norm)
}

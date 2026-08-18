  #' Search similar names from existing data
  #'
  #' maybe() for scientific names, mosiya() for wamei (Japanese names).
  #' Both compare the input against the wamei checklist with
  #' editdist_multi(), and keep the candidates that are close enough.
  #'
  #' @name maybe
  #'
  #' @param x A String or a vector of strings to be checked.
  #' @param len integer 1: when checking scientific name, 6: when checking wamei (Japanese name).
  #' @param min_dist Minimum editing distance. Less than min_dist will be output.
  #' @param min_dist_norm Minimum normalised editing distance.
  #'
  #' @return Tibble.
  #'
  #' @seealso editdist_multi()
  #'
  #' @examples
  #' library(wameicheckr)
  #'
  #' x <- c("Viola madahuricaa", "Carex nevarta")
  #' maybe(x)
  #'
  #' x <-
  #'   c("\u30cf\u30c3\u30ab\u30b0\u30b5",
  #'     "\u30b9\u30ba\u30ce\u30a8\u30f3\u30c9\u30a6") |>
  #'   stringi::stri_unescape_unicode()
  #' mosiya(x)
  #'
NULL

  #' @describeIn maybe Search similar scientific names from existing data
  #' @export
maybe <- function(x, len=1, min_dist=4, min_dist_norm=0.2){
  reference <- ref_sc$name_sc
  res <- 
    editdist_multi(x, reference, inp_esc = TRUE, ref_esc = TRUE, len = len) %>%
    dplyr::filter(editdist < min_dist | editdist_norm < min_dist_norm) %>%
    dplyr::select(-tidyselect::all_of("len")) %>%
    magrittr::set_colnames(c("input", "reference", "editdist", "editdist_norm"))
  dplyr::left_join(res, ref_sc, by=c("reference" = "name_sc")) %>%
    dplyr::distinct()
}

  #' @describeIn maybe Search similar wamei (Japanese names) from existing data
  #' @export
mosiya <- function(x, len=6, min_dist=3, min_dist_norm=0.2){
  reference <- ref_jp$name_jp
  res <- 
    editdist_multi(x, reference, inp_esc = FALSE, ref_esc = TRUE, len = len) %>%
    dplyr::filter(editdist < min_dist | editdist_norm < min_dist_norm) %>%
    dplyr::select(-tidyselect::all_of("len")) %>%
    magrittr::set_colnames(c("input", "reference", "editdist", "editdist_norm"))
  ref_jp <- dplyr::mutate(ref_jp, name_jp = stringi::stri_unescape_unicode(name_jp))
  dplyr::left_join(res, ref_jp, by=c("reference" = "name_jp")) %>%
    dplyr::distinct()
}

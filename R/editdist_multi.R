  #' Compute edit distance
  #' 
  # '@name search_similar_name
  #' 
  #' @param input Vector of string to be compared. 
  #' @param reference Vector of string to be compared. 
  #' @param len integer 1: when checking scientific name, 6: when checking wamei (Japanese name).
  #' @param s1 A string to be compared. 
  #' @param s2 A string to be compared. 
  #' 
  #' @return Tibble. 
  #' 
  #' @seealso editdist()
  #' 
  #' @examples
  #' library(wameicheckr)
  #' library(tidyverse)
  #' library(stringi)
  #' input <- 
  #'    c("\\u30cf\\u30c3\\u30ab\\u30b0\\u30b5", "\\u30b9\\u30ba\\u30ce\\u30a8\\u30f3\\u30c9\\u30a6") %>%
  #'    stringi::stri_unescape_unicode()
  #' reference <- 
  #'    c("\\u30cf\\u30b7\\u30ab\\u30b0\\u30b5", "\\u30b9\\u30ba\\u30e1\\u30ce\\u30a8\\u30f3\\u30c9\\u30a6") %>%
  #'    stringi::stri_unescape_unicode()
  #' editdist_multi(input=input, reference=reference, len = 6)
  #' 
  #' @export
editdist_multi <- function(input, reference, inp_esc = FALSE, ref_esc = FALSE, len = 1L){
  if(len == 6){
    if( ! inp_esc ) input     <- stringi::stri_escape_unicode(input)
    if( ! ref_esc ) reference <- stringi::stri_escape_unicode(reference)
  }
  tidyr::expand_grid(s1=input, s2=reference) %>%
    dplyr::mutate(len = len) %>%
    dplyr::mutate(editdist      = purrr::pmap_int(., editdist)) %>%
    dplyr::mutate(editdist_norm = purrr::pmap_dbl(., editdist_norm)) %>%
    dplyr::mutate_at(c("s1", "s2"), stringi::stri_unescape_unicode)
}


  #' @describeIn editdist_multi Compute normalised edit distance
  #' @export
editdist_norm <- function(s1, s2, editdist, len = 1L){
  editdist / max(stringr::str_length(s1), stringr::str_length(s2)) * len
}

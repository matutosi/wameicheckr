serch_similar <- function(x, len=1, min_dist=4, min_dist_norm=0.2){
  ref <- if(len==6) ref_jp else ref_sc
  res <-
    ref %>%
    dplyr::select(tidyselect::contains("name")) %>%
    .[[1]] %>%
    purrr::map(editdist, x, len=len) %>%
    unlist() %>%
    tibble::tibble(x, ref, dist=.) %>%
    magrittr::set_colnames(c("input", "source", "maybe", "dist")) %>%
      # when wamei (Japanese name)
    if(len==6) dplyr::mutate(., input=stringi::stri_unescape_unicode(input), maybe=stringi::stri_unescape_unicode(maybe)) else . 
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
    ) %>%
    if(nrow(.) > 0) dplyr::rename(., source=tmp) else .
  res
}

maybe <- function(x, len=1, min_dist=4, min_dist_norm=0.2){
  x %>%
    purrr::map(serch_similar, len, min_dist, min_dist_norm) %>%
    dplyr::bind_rows()
}

mosiya <- function(x, len=6, min_dist=3, min_dist_norm=0.2){
  x <- 
    x %>% 
    stringi::stri_trans_general("halfwidth-fullwidth") %>%
    stringi::stri_escape_unicode()
  x %>%
    purrr::map(serch_similar, len, min_dist, min_dist_norm) %>%
    dplyr::bind_rows()
}


x <- c("Viola madahuricaa", "Carex nevarta")
maybe(x)

x <- 
  c("\\u30cf\\u30c3\\u30ab\\u30b0\\u30b5", "\\u30b9\\u30ba\\u30ce\\u30a8\\u30f3\\u30c9\\u30a6") %>%
  stringi::stri_unescape_unicode()
mosiya(x)

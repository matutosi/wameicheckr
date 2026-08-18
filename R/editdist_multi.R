#' Compute edit distance
#' 
# '@name search_similar_name
#' 
#' @param input Vector of string to be compared. 
#' @param reference Vector of string to be compared. 
#' @param inp_esc Logical. TRUE when `input` is already escaped by 
#'   `stringi::stri_escape_unicode()`. Used only when `len` is 6. 
#' @param ref_esc Logical. TRUE when `reference` is already escaped. 
#' @param len integer 1: when checking scientific name, 6: when checking wamei (Japanese name).
#' @param s1 A string to be compared. 
#' @param s2 A string to be compared. 
#' @param editdist Integer. Edit distance of `s1` and `s2`. 
#' 
#' @return Tibble. 
#' 
#' @seealso editdist()
#' 
#' @examples
#' library(wameicheckr)
#' library(stringi)
#' input <- 
#'    c("\\u30cf\\u30c3\\u30ab\\u30b0\\u30b5", 
#'      "\\u30b9\\u30ba\\u30ce\\u30a8\\u30f3\\u30c9\\u30a6") %>%
#'    stringi::stri_unescape_unicode()
#' reference <- 
#'    c("\\u30cf\\u30b7\\u30ab\\u30b0\\u30b5", 
#'      "\\u30b9\\u30ba\\u30e1\\u30ce\\u30a8\\u30f3\\u30c9\\u30a6") %>%
#'    stringi::stri_unescape_unicode()
#' editdist_multi(input = input, reference = reference, len = 6)
#' 
#' @export
editdist_multi <- function(input, reference, 
                           inp_esc = FALSE, 
                           ref_esc = FALSE, 
                           len = 1L){
  if(len == 6){
    if( ! inp_esc ) input     <- stringi::stri_escape_unicode(input)
    if( ! ref_esc ) reference <- stringi::stri_escape_unicode(reference)
  }
  # 列は mutate() を使わずに直接作る．mutate() の中では len が列名として
  # 解決されてしまい，s1 s2 は R CMD check の no visible binding になるため．
  res <- tidyr::expand_grid(s1 = input, s2 = reference)
  res[["len"]]           <- len
  res[["editdist"]]      <- editdist_pairs(input, reference, len)
  res[["editdist_norm"]] <- editdist_norm(res[["s1"]], res[["s2"]],
                                          res[["editdist"]], len)
  res |>
    dplyr::mutate_at(c("s1", "s2"), stringi::stri_unescape_unicode)
}

#' @describeIn editdist_multi Compute normalised edit distance
#' @export
editdist_norm <- function(s1, s2, editdist, len = 1L){
  editdist / pmax(stringr::str_length(s1), stringr::str_length(s2)) * len
}

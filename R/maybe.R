  #' Search similar names from existing data
  #'
  #' maybe() for scientific names, mosiya() for wamei (Japanese names).
  #' Both compare the input against the wamei checklist, and keep the
  #' candidates that are close enough.
  #'
  #' maybe() is the body and mosiya() is a thin wrapper of it.
  #' The two differ only in the defaults: `len` selects the reference
  #' (6 for wamei, otherwise scientific names) and `min_dist` is looser
  #' for scientific names, which are longer.
  #'
  #' The candidates are picked in C++ (`editdist_close_pairs()`), so the
  #' whole `length(x) * length(reference)` table is never built in R.
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
  is_jp <- (len == 6)
  # 参照は len で選ぶ．どちらの参照も source 列を持つ．
  ref <- if(is_jp) ref_jp else ref_sc
  key <- if(is_jp) "name_jp" else "name_sc"
  # 和名(len = 6)はエスケープしたうえで 1 文字 6 バイトとして比べる．
  # ref_jp は既にエスケープして保存してあるので，入力側だけを揃える．
  input <- if(is_jp) stringi::stri_escape_unicode(x) else x

  pairs <- editdist_close_pairs(input, ref[[key]], len = len,
                                min_dist = min_dist,
                                min_dist_norm = min_dist_norm)

  reference <- ref[[key]][pairs[["reference_id"]]]
  if(is_jp){
    reference <- stringi::stri_unescape_unicode(reference)
    ref <- dplyr::mutate(ref, name_jp = stringi::stri_unescape_unicode(.data[["name_jp"]]))
  }
  res <- tibble::tibble(
    input         = x[pairs[["input_id"]]],
    reference     = reference,
    editdist      = pairs[["editdist"]],
    editdist_norm = pairs[["editdist_norm"]])

  # 1 つの和名・学名が複数のデータソースに載るので，結合は意図した多対多．
  by <- key
  names(by) <- "reference"
  dplyr::left_join(res, ref, by = by, relationship = "many-to-many") %>%
    dplyr::distinct()
}

  #' @describeIn maybe Search similar wamei (Japanese names) from existing data
  #' @export
mosiya <- function(x, len=6, min_dist=3, min_dist_norm=0.2){
  maybe(x, len = len, min_dist = min_dist, min_dist_norm = min_dist_norm)
}

  #' wamei_check() と wamei_check_ex() で共通の段階
  #'
  #' 2 つの関数は同じ流れをたどる．違うのは hub の列名(hub_plus と
  #' Hub_name)と，該当なしのときに入れる文字列だけなので，そこを引数にして
  #' 1 つにまとめてある．
  #'
  #' @name wamei_check_parts
  #' @noRd
NULL

  #' 入力ごとに該当件数 n_match を数える
  #' @noRd
wc_count_match <- function(x, hub_master, hub_col){
  x %>%
    dplyr::left_join(hub_master, by = c("input" = "all_name")) %>%
    dplyr::group_by(input) %>%
    dplyr::mutate(n_match = dplyr::n()) %>%
    dplyr::select(input, n_match, {{hub_col}}) %>%
    dplyr::distinct()
}

  #' 該当なし：決まった文字列を入れる
  #' @noRd
wc_no_match <- function(len, hub_col, hub_label, status_label){
  len %>%
    dplyr::filter(is.na({{hub_col}})) %>%
    dplyr::transmute(input,
                     n_match = 0,
                     "{{hub_col}}" := hub_label,
                     status = status_label)
}

  #' 該当なしを除く
  #' @noRd
wc_drop_no_match <- function(len, hub_col){
  len %>%
    dplyr::filter(!is.na({{hub_col}})) %>%
    dplyr::distinct(input, n_match)
}

  #' 1 件合致の結果を横長にする
  #' @noRd
wc_widen_single <- function(single_match, hub_col){
  single_match %>%
    tidyr::pivot_wider(
      id_cols = c(input, n_match, {{hub_col}}, status, dplyr::starts_with("Family")),
      names_from = source,
      values_from = c(ID, common_name, dplyr::starts_with("scientific")),
      names_glue = "{source}_{.value}"
    )
}

  #' 0 件・1 件・2 件以上の結果を入力の順に並べ直す
  #' @noRd
wc_bind_results <- function(x, no_match, multi_match, single_match){
  x %>%
    dplyr::left_join(dplyr::bind_rows(no_match, multi_match, single_match), by = "input")
}

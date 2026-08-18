  #' 和名チェックリストから和名・学名の候補を出力
  #' 
  #' 和名チェックリストを使用して，和名・学名の候補を出力．
  #' 和名チェックリスト
  #'     https://www.gbif.jp/v2/activities/wamei_checklist.html
  ## エクセル版「維管束植物和名変換シート」の機能を再現
  #'     https://wetlands.info/tools/plantsdb/nameconv/
  #' 
  #' @param x 検索する和名の文字列のベクトル．
  #' @param hub_master 和名チェックリストの「Hub_data」シートのデータ
  #' @param jn_master 和名チェックリストの「JN_dataset」シートのデータ
  #' @param wide 論理値：出力形式
  #' 
  #' @return tibble形式．検索した和名をキーとして，和名・学名など．
  #' 維管束植物和名変換シート(excel版)と同等．
  #'     https://wetlands.info/tools/plantsdb/nameconv/
  #' 
  #' @seealso wamei_check()
  #' 
  #' @examples
  #' # see vignette
  #' # vignette("wamei_checkr")
  #' 
  #' @export
wamei_check_ex <- function(
    x,              # String or Vector to be checked
    hub_master,     # hub data
    jn_master,      # jn data
    wide = TRUE     # output style
  ){
  x          <- tibble::tibble(input = x)
  hub_master <- clean_colnames(hub_master)
  jn         <- wcex_jn_table(jn_master)
  # hub_master を用途ごとに分ける
  msg      <- wcex_message_table(hub_master)
  hub_name <- wcex_hub_name_table(hub_master)
  id       <- wcex_id_table(hub_master)
  stts     <- wcex_status_table(hub_master)
  # 該当件数で 0 件・1 件・2 件以上に振り分ける
  len          <- wc_count_match(x, hub_master, Hub_name)
  no_match     <- wcex_no_match(len)
  len          <- wc_drop_no_match(len, Hub_name)
  multi_match  <- wcex_multi_match(len, msg)
  single_match <- wcex_single_match(len, hub_name, stts, id, jn)
  if(wide & nrow(single_match) > 0) single_match <- wc_widen_single(single_match, Hub_name)
  wcex_finish(x, no_match, multi_match, single_match)
}

  #' jn シートから 1 つの ID につき先頭の行だけを取り出す
  #' @noRd
wcex_jn_table <- function(jn_master){
  jn_master %>%
    clean_colnames() %>%
    dplyr::distinct(ID, .keep_all = TRUE) %>%
    dplyr::select(! dplyr::starts_with(c("another", "note")))
}

  #' message を分離する
  #' @noRd
wcex_message_table <- function(hub_master){
  hub_master %>%
    dplyr::select(all_name, message) %>%
    dplyr::distinct(all_name, .keep_all = TRUE)
}

  #' Hub_name を分離する
  #' @noRd
wcex_hub_name_table <- function(hub_master){
  hub_master %>%
    dplyr::select(all_name, Hub_name) %>%
    dplyr::distinct(all_name, .keep_all = TRUE)
}

  #' データソースごとの ID を縦長にする
  #' @noRd
wcex_id_table <- function(hub_master){
  hub_master %>%
    dplyr::select(all_name, GL:YL) %>%
    dplyr::distinct(all_name, .keep_all = TRUE) %>%
    tidyr::pivot_longer(cols = GL:YL, names_to = "source", values_to = "ID", values_drop_na = TRUE)
}

  #' status を分離する
  #' @noRd
wcex_status_table <- function(hub_master){
  hub_master %>%
    dplyr::select(all_name, status) %>%
    dplyr::distinct(all_name, .keep_all = TRUE)
}

  #' 該当なし：message を表示する
  #' @noRd
wcex_no_match <- function(len){
  len %>%
    dplyr::filter(is.na(Hub_name)) %>%
    # "\uff01\u5019\u88dc\u306a\u3057" は「！候補なし」，"\uff01\u500b\u5225\u306b\u691c\u8a0e" は「！個別に検討」
    dplyr::transmute(input, n_match = 0, Hub_name = "\uff01\u5019\u88dc\u306a\u3057", status = "\uff01\u500b\u5225\u306b\u691c\u8a0e")
}

  #' 2 つ以上が合致した和名は message を出す
  #' @noRd
wcex_multi_match <- function(len, msg){
  len %>%
    dplyr::filter(n_match>1)  %>%
    dplyr::transmute(input, n_match, status = "\uff01\u500b\u5225\u306b\u691c\u8a0e") %>%
    dplyr::distinct() %>%
    dplyr::left_join(msg, by = c("input" = "all_name")) %>%
    dplyr::rename(Hub_name = message)
}

  #' 1 つだけ合致した和名に情報を付ける
  #' @noRd
wcex_single_match <- function(len, hub_name, stts, id, jn){
  len %>%
    dplyr::filter(n_match == 1) %>%
    dplyr::transmute(input, n_match) %>%
    dplyr::left_join(hub_name, by = c("input" = "all_name")) %>%
    dplyr::left_join(stts,     by = c("input" = "all_name")) %>%
    dplyr::left_join(id,       by = c("input" = "all_name")) %>%
    dplyr::left_join(jn,       by = "ID")
}

  #' 3 つの結果を統合して返す
  #' @noRd
wcex_finish <- function(x, no_match, multi_match, single_match){
  x %>%
    dplyr::left_join(dplyr::bind_rows(no_match, multi_match, single_match), by = "input")
}

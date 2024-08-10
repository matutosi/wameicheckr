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
  # # # # # # # # # # # preparations # # # # # # # # # # # 
  hub_master <-          # modify column names
    hub_master %>% 
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_remove_all(., "[()]"))
  jn_master <-           # modify column names
    jn_master %>% 
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_remove_all(., "[()]"))
  msg <-          # divide message alone
    hub_master %>%
    dplyr::select(all_name, message) %>%
    dplyr::distinct(all_name, .keep_all = TRUE)
  hub_name <-     # divide hub
    hub_master %>%
    dplyr::select(all_name, Hub_name) %>%
    dplyr::distinct(all_name, .keep_all = TRUE)
  id <-            # divide id, longer style
    hub_master %>%
    dplyr::select(all_name, GL:YL) %>%
    dplyr::distinct(all_name, .keep_all = TRUE) %>%
    tidyr::pivot_longer(cols = GL:YL, names_to = "source", values_to = "ID", values_drop_na = TRUE)
  stts <-           # divide status
    hub_master %>%
    dplyr::select(all_name, status) %>%
    dplyr::distinct(all_name, .keep_all = TRUE)
  jn_master <-             # keep only upper one row & another_name_ID==0
    jn_master %>%
    dplyr::distinct(ID, .keep_all = TRUE) %>%
    dplyr::select(! dplyr::starts_with(c("another", "note")))
  # # # # # # # # # # # main part # # # # # # # # # # # 
  len <-            # number of matches
    tibble::tibble(input = x) %>%
    dplyr::left_join(hub_master, by = c("input" = "all_name")) %>%
    dplyr::group_by(input) %>%
    dplyr::mutate(n_match = dplyr::n()) %>%
    dplyr::select(input, n_match, Hub_name) %>%
    dplyr::distinct()
  no_match <-        # show message
    len %>%
    dplyr::filter(is.na(Hub_name)) %>%
    dplyr::transmute(input, n_match = 0, Hub_name = "！候補なし", status = "！個別に検討")
  len <-             # remove no match
    len %>%
    dplyr::filter(!is.na(Hub_name) ) %>%
    dplyr::distinct(input, n_match)
  multi_match <-     # match > 1
    len %>%
    dplyr::filter(n_match>1)  %>%
    dplyr::transmute(input, n_match, status = "！個別に検討") %>%
    dplyr::distinct() %>%
    dplyr::left_join(msg, by = c("input" = "all_name")) %>%
    dplyr::rename(Hub_name = message)
  single_match <-    # match == 1
    len %>%
    dplyr::filter(n_match == 1) %>%
    dplyr::transmute(input, n_match) %>%
    dplyr::left_join(hub_name,  by = c("input" = "all_name")) %>%
    dplyr::left_join(stts,      by = c("input" = "all_name")) %>%
    dplyr::left_join(id,        by = c("input" = "all_name")) %>%
    dplyr::left_join(jn_master, by = "ID")
  # longer style
  if(wide & nrow(single_match)>0){
    single_match <-   
      single_match %>%
      tidyr::pivot_wider(
        id_cols = c(input, n_match, Hub_name, status, dplyr::starts_with("Family")), 
        names_from = source, 
        values_from = c(ID, common_name, dplyr::starts_with("scientific")),
        names_glue = "{source}_{.value}"
      )
  }
  # # # # # # # # # # # output # # # # # # # # # # # 
  res <- 
    tibble::tibble(input = x) %>%
    dplyr::left_join(dplyr::bind_rows(no_match, multi_match, single_match), by = "input")
  res
}

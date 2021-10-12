  #' 和名チェックリストから和名・学名の候補を出力
  #' 
  #' 和名チェックリストを使用して，和名・学名の候補を出力します．
  #' 和名チェックリスト
  #'     https://www.gbif.jp/v2/activities/wamei_checklist.html
  #' 
  #' @param x 検索する和名の文字列のベクトル．
  #' @param hub_master 和名チェックリストの「Hub_data」シートのデータ
  #' @param jn_master 和名チェックリストの「JN_dataset」シートのデータ
  #' @param wide 論理値：出力形式
  #' @param ds データソース
  #' 
  #' @return tibble形式．検索した和名をキーとして，和名・学名など．
  #' 維管束植物和名変換シート(excel版)に加えて，全ての和名・学名の
  #' 候補を出力．
  #'     https://wetlands.info/tools/plantsdb/nameconv/
  #' 
  #' @seealso wamei_check_ex()
  #' 
  #' @examples
  #' # see vignette
  #' # vignette("wamei_checkr")
  #' 
  #' @export
wamei_check <- function(  # 和名チェク(エクセルを改変)
    x,                         # チェックする和名(string or vector)
    hub_master,                # hubシート
    jn_master,                 # jnシート
    wide = TRUE,               # 出力形式
    ds   = c(GL, SF, WF, YL)   # 使用するデータソース
  ){
   # # # # # # # # # # # 準備 # # # # # # # # # # # 
  x <- tibble::tibble(input = x)
  jn_master <-      # 列名の修正
    jn_master %>% 
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[()]", ""))
  jn <-             # another_name_ID == 0
    jn_master %>%
    dplyr::filter(another_name_ID == 0) %>%
    dplyr::select(! dplyr::starts_with(c("another", "note", "Family"))) %>%
    dplyr::distinct() # 本来は不要?
  hub_master <-      # 列名の修正
    hub_master %>% 
    dplyr::filter(all_name %in% x$input) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[()]", "")) %>%
    dplyr::filter(dplyr::if_any({{ds}}, ~!is.na(.x))) %>%
  #     select_ds(hub_master = ., ds = {{ds}}) %>%
    dplyr::mutate(hub_plus = hub2plus(Hub_name, lato_stricto)) %>%
    dplyr::distinct() # 本来は不要?
  msg <-          # messageを自動生成
    hub_master %>%
    dplyr::select(all_name, hub_plus) %>%
    dplyr::group_by(all_name) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::mutate(msg = "message") %>%
    tidyr::pivot_wider(
      id_cols = all_name, names_from = msg, values_from = hub_plus, 
      values_fn = list(hub_plus = ~paste(., collapse = "；"))
    ) %>%
    dplyr::mutate(message = arrange_hub_name(message)) %>%
    dplyr::mutate(message = stringr::str_replace(message, "^/", "")) %>%
    dplyr::mutate(message = stringr::str_replace_all(message, "/+", "/"))
  id <-            # idの分離・縦長に
    hub_master %>%
    dplyr::select(all_name,    {{ds}}) %>%
    tidyr::pivot_longer(cols = {{ds}}, names_to = "source", values_to = "ID", values_drop_na = TRUE) %>%
    dplyr::distinct() # 本来は不要のはず
  stts <-           # statusを分離・縦長に
    hub_master %>%
    dplyr::select(all_name, status)
  hub <-     # hubを分離
    hub_master %>%
    dplyr::select(all_name, hub_plus) %>%
    dplyr::distinct()
  fml <-     # familyを分離
    hub_master %>%
    dplyr::select(all_name, dplyr::starts_with("Family")) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      id_cols = all_name, values_from = dplyr::starts_with("Family"), 
       values_fn = function(x) {paste(x, collapse = "；")},  names_glue = "{.value}" 
     )
  # # # # # # # # # # # メイン # # # # # # # # # # # 
  len <-            # 合致した数
    x %>%
    dplyr::left_join(hub_master, by = c("input" = "all_name")) %>%
    dplyr::group_by(input) %>%
    dplyr::mutate(n_match = dplyr::n()) %>%
    dplyr::select(input, n_match, hub_plus) %>%
    dplyr::distinct()
  no_match <-        # 該当なし：messageを表示
    len %>%
    dplyr::filter(is.na(hub_plus)) %>%
    dplyr::transmute(input, n_match = 0, hub_plus = "該当なし", status = "該当なし")
  len <-             # 該当なしを除去
    len %>%
    dplyr::filter(!is.na(hub_plus) ) %>%
    dplyr::distinct(input, n_match)
  single_match <-    # 1つだけ合致
    len %>%
    dplyr::filter(n_match == 1) %>%
    dplyr::left_join(hub,  by = c("input" = "all_name")) %>%  # hub 以外は single/multi共通
    dplyr::left_join(stts, by = c("input" = "all_name")) %>%
    dplyr::left_join(id,   by = c("input" = "all_name")) %>%
    dplyr::left_join(fml,  by = c("input" = "all_name")) %>%
    dplyr::left_join(jn,   by = "ID")
  multi_match <-     # 2つ以上が合致
    len %>%
    dplyr::filter(n_match>1)  %>%
    dplyr::left_join(msg,  by = c("input" = "all_name")) %>%  # msg 以外は single/multi共通
    dplyr::left_join(stts, by = c("input" = "all_name")) %>%
    dplyr::left_join(id,   by = c("input" = "all_name")) %>%
    dplyr::left_join(fml,  by = c("input" = "all_name")) %>%
    dplyr::left_join(jn,   by = "ID") %>%
    dplyr::rename(hub_plus = message) %>%  # 他と合わせる
    dplyr::distinct()
  # # # # # # # # # # # 横長変換(オプション) # # # # # # # # # # # 
  if(wide & nrow(single_match)>0){
    single_match <-   
      single_match %>%
      tidyr::pivot_wider(
        id_cols = c(input, n_match, hub_plus, status, dplyr::starts_with("Family")), 
        names_from = source, 
        values_from = c(ID, common_name, dplyr::starts_with("scientific")),
        names_glue = "{source}_{.value}"
      )
  }
  if(wide & nrow(multi_match)>0){
    multi_match <-   
      multi_match %>%
      tidyr::pivot_wider(
        id_cols = c(input, n_match, hub_plus, status, dplyr::starts_with("Family")), 
        names_from = source, 
        values_from = c(ID, common_name, scientific_name_with_author, scientific_name_without_author),
        names_glue = "{source}_{.value}", 
        values_fn = list(
          ID                             = ~paste(., collapse = "；"),
          common_name                    = ~paste(., collapse = "；"),
          scientific_name_with_author    = ~paste(., collapse = "；"),
          scientific_name_without_author = ~paste(., collapse = "；")
        )
      )  %>%
      dplyr::mutate_at(dplyr::vars(dplyr::contains("common_name")), arrange_hub_name) %>%  # vars() は必須
      dplyr::mutate(st = "status") %>%
      tidyr::pivot_wider(
        names_from = st, 
        values_from = status,
        values_fn = list(status = ~paste(., collapse = "；"))
      )
  }
  # # # # # # # # # # # 結果の統合・並べ替え・出力 # # # # # # # # # # # 
  res <- 
    x %>%
    dplyr::left_join(dplyr::bind_rows(no_match, multi_match, single_match), by = "input") %>%
    dplyr::mutate(hub_plus = stringr::str_replace_all(hub_plus, "-", "")) %>%
    dplyr::mutate_all(tidyr::replace_na, "") %>%
    dplyr::mutate_all(stringr::str_replace_all, "^$", "-")
  if(wide){
    res <- dplyr::select(res, !any_of(c("source", "ID", "common_name", "scientific_name_with_author", "scientific_name_without_author")))
  }
  res
}

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
  x          <- tibble::tibble(input = x)
  jn         <- wc_jn_table(jn_master)
  hub_master <- wc_hub_table(hub_master, x, {{ds}})
  # hub_master を用途ごとに分ける
  msg  <- wc_message_table(hub_master)
  id   <- wc_id_table(hub_master, {{ds}})
  stts <- wc_status_table(hub_master)
  hub  <- wc_hub_plus_table(hub_master)
  fml  <- wc_family_table(hub_master)
  # 該当件数で 0 件・1 件・2 件以上に振り分ける
  len          <- wc_count_match(x, hub_master, "hub_plus")
  no_match     <- wc_no_match(len, "hub_plus",
                              "\u8a72\u5f53\u306a\u3057",   # 該当なし
                              "\u8a72\u5f53\u306a\u3057")   # 該当なし
  len          <- wc_drop_no_match(len, "hub_plus")
  single_match <- wc_single_match(len, hub, stts, id, fml, jn)
  multi_match  <- wc_multi_match(len, msg, stts, id, fml, jn)
  if(wide & nrow(single_match) > 0) single_match <- wc_widen_single(single_match, "hub_plus")
  if(wide & nrow(multi_match)  > 0) multi_match  <- wc_widen_multi(multi_match)
  wc_finish(x, no_match, multi_match, single_match, wide)
}

  #' jn シートから another_name_ID == 0 の行だけを取り出す
  #' @noRd
wc_jn_table <- function(jn_master){
  jn_master %>%
    clean_colnames() %>%
    dplyr::filter(.data[["another_name_ID"]] == 0) %>%
    dplyr::select(! dplyr::starts_with(c("another", "note", "Family"))) %>%
    dplyr::distinct() # 本来は不要?
}

  #' hub シートを入力の和名で絞り，hub_plus を付ける
  #' @noRd
wc_hub_table <- function(hub_master, x, ds){
  hub_master %>%
    dplyr::filter(.data[["all_name"]] %in% x$input) %>%
    clean_colnames() %>%
    dplyr::filter(dplyr::if_any({{ds}}, ~!is.na(.x))) %>%
    dplyr::mutate(hub_plus = hub2plus(.data[["Hub_name"]], .data[["lato_stricto"]])) %>%
    dplyr::distinct() # 本来は不要?
}

  #' 2 件以上該当する和名について message を自動生成する
  #' @noRd
wc_message_table <- function(hub_master){
  msg <-
    hub_master %>%
    dplyr::select("all_name", "hub_plus") %>%
    dplyr::group_by(.data[["all_name"]]) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::mutate(msg = "message") %>%
    tidyr::pivot_wider(
      id_cols = "all_name", names_from = "msg", values_from = "hub_plus",
      values_fn = list(hub_plus = ~paste(., collapse = "\uff1b"))
    )
  # 2 件以上該当する和名が 1 つも無いと，pivot_wider() は message 列を作らない
  if(! "message" %in% names(msg)) msg[["message"]] <- character(nrow(msg))
  msg %>%
    dplyr::mutate(message = arrange_hub_name(.data[["message"]])) %>%
    dplyr::mutate(message = stringr::str_remove(.data[["message"]], "^/")) %>%
    dplyr::mutate(message = stringr::str_replace_all(.data[["message"]], "/+", "/"))
}

  #' データソースごとの ID を縦長にする
  #' @noRd
wc_id_table <- function(hub_master, ds){
  hub_master %>%
    dplyr::select("all_name", {{ds}}) %>%
    tidyr::pivot_longer(cols = {{ds}}, names_to = "source", values_to = "ID", values_drop_na = TRUE) %>%
    dplyr::distinct() # 本来は不要のはず
}

  #' status を分離する
  #' @noRd
wc_status_table <- function(hub_master){
  hub_master %>%
    dplyr::select("all_name", "status")
}

  #' hub_plus を分離する
  #' @noRd
wc_hub_plus_table <- function(hub_master){
  hub_master %>%
    dplyr::select("all_name", "hub_plus") %>%
    dplyr::distinct()
}

  #' Family 系の列を 1 行にまとめる
  #' @noRd
wc_family_table <- function(hub_master){
  hub_master %>%
    dplyr::select("all_name", dplyr::starts_with("Family")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(tmp="") %>%
    tidyr::pivot_wider(
      id_cols = "all_name",
      names_from = "tmp",
      values_from = dplyr::starts_with("Family"),
      values_fn = function(x) {paste(x, collapse = "\uff1b")},
      names_glue = "{.value}"
    )
}

  #' 1 つだけ合致した和名に情報を付ける
  #' @noRd
wc_single_match <- function(len, hub, stts, id, fml, jn){
  len %>%
    dplyr::filter(.data[["n_match"]] == 1) %>%
    dplyr::left_join(hub,  by = c("input" = "all_name")) %>%  # hub 以外は single/multi共通
    dplyr::left_join(stts, by = c("input" = "all_name")) %>%
    dplyr::left_join(id,   by = c("input" = "all_name")) %>%
    dplyr::left_join(fml,  by = c("input" = "all_name")) %>%
    dplyr::left_join(jn,   by = "ID")
}

  #' 2 つ以上が合致した和名に情報を付ける
  #' @noRd
wc_multi_match <- function(len, msg, stts, id, fml, jn){
  len %>%
    dplyr::filter(.data[["n_match"]] > 1)  %>%
    dplyr::left_join(msg,  by = c("input" = "all_name")) %>%  # msg 以外は single/multi共通
    dplyr::left_join(stts, by = c("input" = "all_name")) %>%
    dplyr::left_join(id,   by = c("input" = "all_name")) %>%
    dplyr::left_join(fml,  by = c("input" = "all_name")) %>%
    dplyr::left_join(jn,   by = "ID") %>%
    dplyr::rename(hub_plus = "message") %>%  # 他と合わせる
    dplyr::distinct()
}

  #' 2 件以上合致の結果を横長にする
  #' @noRd
wc_widen_multi <- function(multi_match){
  multi_match %>%
    tidyr::pivot_wider(
      id_cols = c("input", "n_match", "hub_plus", "status", dplyr::starts_with("Family")),
      names_from = "source",
      values_from = c("ID", "common_name", "scientific_name_with_author", "scientific_name_without_author"),
      names_glue = "{source}_{.value}",
      values_fn = list(
        ID                             = ~paste(., collapse = "\uff1b"),
        common_name                    = ~paste(., collapse = "\uff1b"),
        scientific_name_with_author    = ~paste(., collapse = "\uff1b"),
        scientific_name_without_author = ~paste(., collapse = "\uff1b")
      )
    )  %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("common_name")), arrange_hub_name) %>%  # vars() は必須
    dplyr::mutate(st = "status") %>%
    tidyr::pivot_wider(
      names_from = "st",
      values_from = "status",
      values_fn = list(status = ~paste(., collapse = "\uff1b"))
    )
}

  #' 3 つの結果を統合し，空欄を整えて返す
  #' @noRd
wc_finish <- function(x, no_match, multi_match, single_match, wide){
  res <-
    wc_bind_results(x, no_match, multi_match, single_match) %>%
    dplyr::mutate(hub_plus = stringr::str_remove_all(.data[["hub_plus"]], "-")) %>%
    dplyr::mutate_if(is.character, tidyr::replace_na, "") %>%
    dplyr::mutate_if(is.character, stringr::str_replace_all, "^$", "-")
  if(wide){
    res <- dplyr::select(res, !tidyselect::any_of(c("source", "ID", "common_name", "scientific_name_with_author", "scientific_name_without_author")))
  }
  res
}

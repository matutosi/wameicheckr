## 和名チェックリストのエクセル版の機能を再現
wamei_check_excel2 <- function(
    x,              # チェックする和名(string or vector)
    hub,            # hubシート
    jn,             # jnシート
    wide=TRUE       # 出力形式
  ){
  # # # # # # # # # # # 準備 # # # # # # # # # # # 
  hub <-          # 列名の修正
    hub %>% 
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[()]", ""))
  jn <-           # 列名の修正
    jn %>% 
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[()]", ""))
  msg <-          # messageを分離
    hub %>%
    dplyr::select(all_name, message) %>%
    dplyr::distinct(all_name, .keep_all=TRUE)
  hub_name <-     # hubを分離
    hub %>%
    dplyr::select(all_name, Hub_name) %>%
    dplyr::distinct(all_name, .keep_all=TRUE)
  id <-            # idの分離・縦長に
    hub %>%
    dplyr::select(all_name, GL:YL) %>%
    dplyr::distinct(all_name, .keep_all=TRUE) %>%
    tidyr::pivot_longer(cols=GL:YL, names_to="source", values_to="ID", values_drop_na=TRUE)
  stts <-           # statusを分離・縦長に
    hub %>%
    dplyr::select(all_name, status) %>%
    dplyr::distinct(all_name, .keep_all=TRUE)
  jn <-             # another_name_IDが0とか一番上のものだけ残す
    jn %>%
    dplyr::distinct(ID, .keep_all=TRUE) %>%
    dplyr::select(! starts_with(c("another", "note")))
  # # # # # # # # # # # メイン # # # # # # # # # # # 
  len <-            # 合致した数
    tibble(input=x) %>%
    dplyr::left_join(hub, by=c("input"="all_name")) %>%
    dplyr::group_by(input) %>%
    dplyr::mutate(n_match=n()) %>%
    dplyr::select(input, n_match, Hub_name) %>%
    dplyr::distinct()
  no_match <-        # 該当なし：messageを表示
    len %>%
    dplyr::filter(is.na(Hub_name)) %>%
    dplyr::transmute(input, n_match=0, Hub_name="！候補なし", status="！個別に検討")
  len <-             # 該当なしを除去
    len %>%
    dplyr::filter(!is.na(Hub_name) ) %>%
    dplyr::distinct(input, n_match)
  multi_match <-     # 2つ以上が合致
    len %>%
    dplyr::filter(n_match>1)  %>%
    dplyr::transmute(input, n_match, status="！個別に検討") %>%
    dplyr::distinct() %>%
    dplyr::left_join(msg, by=c("input"="all_name")) %>%
    dplyr::rename(Hub_name=message)
  single_match <-    # 1つだけ合致
    len %>%
    dplyr::filter(n_match==1) %>%
    dplyr::transmute(input, n_match) %>%
    dplyr::left_join(hub_name, by=c("input"="all_name")) %>%
    dplyr::left_join(stts,     by=c("input"="all_name")) %>%
    dplyr::left_join(id,       by=c("input"="all_name")) %>%
    dplyr::left_join(jn,       by="ID")
  # 横長に
  if(wide & nrow(single_match)>0){
    single_match <-   
      single_match %>%
      tidyr::pivot_wider(
        id_cols=c(input, n_match, Hub_name, status, starts_with("Family")), 
        names_from=source, 
        values_from=c(ID, common_name, starts_with("scientific")),
        names_glue="{source}_{.value}"
      )
  }
  # # # # # # # # # # # 結果の統合・並べ替え・出力 # # # # # # # # # # # 
  res <- 
    tibble(input=x) %>%
    dplyr::left_join(dplyr::bind_rows(no_match, multi_match, single_match), by="input")
  res
}
## 和名チェク(エクセルを改変)
wamei_check <- function(
    x,                         # チェックする和名(string or vector)
    hub_master,                # hubシート
    jn_master,                 # jnシート
    wide = TRUE,               # 出力形式
    ds   = c(GL, SF, WF, YL),  # 使用するデータソース
    ...
  ){
   # # # # # # # # # # # 準備 # # # # # # # # # # # 
  x <- tibble(input=x)
  jn_master <-      # 列名の修正
    jn_master %>% 
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[()]", ""))
  jn <-             # another_name_ID==0
    jn_master %>%
    dplyr::filter(another_name_ID==0) %>%
    dplyr::select(! starts_with(c("another", "note", "Family"))) %>%
    dplyr::distinct() # 本来は不要?
  hub_master <-      # 列名の修正
    hub_master %>% 
    dplyr::filter(all_name %in% x$input) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[()]", "")) %>%
    dplyr::filter(if_any({{ds}}, ~!is.na(.x))) %>%
  #     select_ds(hub_master=., ds={{ds}}) %>%
    dplyr::mutate(hub_plus = hub2plus(Hub_name, lato_stricto)) %>%
    dplyr::distinct() # 本来は不要?
  msg <-          # messageを自動生成
    hub_master %>%
    dplyr::select(all_name, hub_plus) %>%
    dplyr::group_by(all_name) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::mutate(msg="message") %>%
    tidyr::pivot_wider(
      id_cols=all_name, names_from=msg, values_from=hub_plus, 
      values_fn=list(hub_plus=~paste(., collapse = "；"))
    ) %>%
    dplyr::mutate(message = arrange_hub_name(message)) %>%
    dplyr::mutate(message = stringr::str_replace(message, "^/", "")) %>%
    dplyr::mutate(message = stringr::str_replace_all(message, "/+", "/"))
  id <-            # idの分離・縦長に
    hub_master %>%
    dplyr::select(all_name,    {{ds}}) %>%
    tidyr::pivot_longer(cols = {{ds}}, names_to="source", values_to="ID", values_drop_na=TRUE) %>%
    dplyr::distinct() # 本来は不要のはず
  stts <-           # statusを分離・縦長に
    hub_master %>%
    dplyr::select(all_name, status)
  hub <-     # hubを分離
    hub_master %>%
    dplyr::select(all_name, hub_plus) %>%
    distinct()
  fml <-     # familyを分離
    hub_master %>%
    dplyr::select(all_name, starts_with("Family")) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      id_cols=all_name, values_from=starts_with("Family"), 
       values_fn = function(x) {paste(x, collapse = "；")},  names_glue="{.value}" 
     )
  # # # # # # # # # # # メイン # # # # # # # # # # # 
  len <-            # 合致した数
    x %>%
    dplyr::left_join(hub_master, by=c("input"="all_name")) %>%
    dplyr::group_by(input) %>%
    dplyr::mutate(n_match=n()) %>%
    dplyr::select(input, n_match, hub_plus) %>%
    dplyr::distinct()
  no_match <-        # 該当なし：messageを表示
    len %>%
    dplyr::filter(is.na(hub_plus)) %>%
    dplyr::transmute(input, n_match=0, hub_plus="該当なし", status="該当なし")
  len <-             # 該当なしを除去
    len %>%
    dplyr::filter(!is.na(hub_plus) ) %>%
    dplyr::distinct(input, n_match)
  single_match <-    # 1つだけ合致
    len %>%
    dplyr::filter(n_match==1) %>%
    dplyr::left_join(hub,  by=c("input"="all_name")) %>%  # hub 以外は single/multi共通
    dplyr::left_join(stts, by=c("input"="all_name")) %>%
    dplyr::left_join(id,   by=c("input"="all_name")) %>%
    dplyr::left_join(fml,  by=c("input"="all_name")) %>%
    dplyr::left_join(jn,   by="ID")
  multi_match <-     # 2つ以上が合致
    len %>%
    dplyr::filter(n_match>1)  %>%
    dplyr::left_join(msg,  by=c("input"="all_name")) %>%  # msg 以外は single/multi共通
    dplyr::left_join(stts, by=c("input"="all_name")) %>%
    dplyr::left_join(id,   by=c("input"="all_name")) %>%
    dplyr::left_join(fml,  by=c("input"="all_name")) %>%
    dplyr::left_join(jn,   by="ID") %>%
    dplyr::rename(hub_plus=message) %>%  # 他と合わせる
    dplyr::distinct()
  # # # # # # # # # # # 横長変換(オプション) # # # # # # # # # # # 
  if(wide & nrow(single_match)>0){
    single_match <-   
      single_match %>%
      tidyr::pivot_wider(
        id_cols=c(input, n_match, hub_plus, status, starts_with("Family")), 
        names_from=source, 
        values_from=c(ID, common_name, starts_with("scientific")),
        names_glue="{source}_{.value}"
      )
  }
  if(wide & nrow(multi_match)>0){
    multi_match <-   
      multi_match %>%
      tidyr::pivot_wider(
        id_cols=c(input, n_match, hub_plus, status, starts_with("Family")), 
        names_from=source, 
        values_from=c(ID, common_name, scientific_name_with_author, scientific_name_without_author),
        names_glue="{source}_{.value}", 
        values_fn = list(
          ID                             = ~paste(., collapse = "；"),
          common_name                    = ~paste(., collapse = "；"),
          scientific_name_with_author    = ~paste(., collapse = "；"),
          scientific_name_without_author = ~paste(., collapse = "；")
        )
      )  %>%
      dplyr::mutate_at(vars(contains("common_name")), arrange_hub_name) %>%  # vars() は必須
      dplyr::mutate(st="status") %>%
      tidyr::pivot_wider(
        names_from=st, 
        values_from=status,
        values_fn = list(status = ~paste(., collapse = "；"))
      )
  }
  # # # # # # # # # # # 結果の統合・並べ替え・出力 # # # # # # # # # # # 
  res <- 
    x %>%
    dplyr::left_join(dplyr::bind_rows(no_match, multi_match, single_match), by="input") %>%
    dplyr::mutate(hub_plus=stringr::str_replace_all(hub_plus, "-", "")) %>%
    dplyr::mutate_all(tidyr::replace_na, "") %>%
    dplyr::mutate_all(stringr::str_replace_all, "^$", "-")
  res
}
## /を含むmessageの整理
arrange_hub_name <- function(x){
  # 入力が空の場合
  if(! is.character(x)) return("")
  # 個別の和名を分解
  #   x <-
  #     x %>%
  #     stringr::str_split("，|；|/") %>%
  #     purrr::map(sort) %>%
  #     purrr::map(unique)
  x <-
    x %>%
    stringr::str_split("，|；|/") %>%
    purrr::map(sort) %>%
    purrr::map(unique) %>%
    purrr::map(function(x) if(length(x)==0) "" else x)
  # tibbleにしてから，case_whenやreduceなどの処理
  x <-
    x %>%
    purrr::map(~tibble::tibble(hub_plus=.)) %>%
    purrr::map(~tidyr::separate( ., hub_plus, into=c("hub", "plus"), sep="-", fill="right")) %>%
    purrr::map(~dplyr::transmute(., 
      hub_plus = dplyr::case_when( hub == dplyr::lag(hub) ~ plus, TRUE ~ paste(hub, plus, sep="")), 
      hub_plus = purrr::reduce(hub_plus, ~stringr::str_c(.x, "/", .y))    )) %>%
    purrr::map(distinct) %>%
    unlist()
  # 細かな修正
  x %>%
    `names<-`(NULL) %>%
    stringr::str_replace_all("NA", "")
}
## hub_plusを作る「/」での区切りでも前後両方に「lato_stricto」を追加
hub2plus <- function(Hub_name, lato_stricto){
  hub_name <- 
    Hub_name %>% 
    stringr::str_split("/")
  purrr::map2(hub_name, lato_stricto, paste, sep="-", collapse="/") %>%
    stringr::str_replace_all("-NA", "")
}
## another_name_IDの空欄を自動的に埋めてしまう．
fill_another_name_id <- function(jn){
  # fill_another_name_id がOKのもの
  an_id_ok <- 
    jn %>%
    tibble::rownames_to_column("row_num") %>%
    dplyr::mutate(another_name_ID=as.numeric(another_name_ID)) %>%
    dplyr::filter(! is.na(another_name_ID))
  # fill_another_name_id がNGのもの
  an_id_ng <- 
    jn %>%
    tibble::rownames_to_column("row_num") %>%
    dplyr::filter(is.na(another_name_ID)) %>%
    dplyr::mutate(ID2=lag(ID, default="")) %>%
    dplyr::mutate(another_name_ID = dplyr::case_when(
      ID!=ID2 ~ 0,
      TRUE    ~ 1
    )) %>%
    dplyr::mutate(another_name_ID=purrr::accumulate(another_name_ID, function(x,y){ (x+1)* y} )) %>%
    dplyr::select(-ID2)
  # 統合して出力
  dplyr::bind_rows(an_id_ok, an_id_ng) %>%
    dplyr::mutate(row_num=as.numeric(row_num)) %>%
    dplyr::arrange(row_num) %>%
    dplyr::select(-row_num)
}

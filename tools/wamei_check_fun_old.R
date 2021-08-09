## データソースを選択して抽出
  # filter(if_any(, ~!is.na(.x)))で可能なのが分かったので，削除
select_ds <- function(hub_master, ds){
  hub_master %>%
  dplyr::select({{ds}}) %>%
  dplyr::mutate_all(is.na) %>%
  dplyr::mutate_all(`!`) %>%                  # 上で一気に !is.na はエラーになるため
  dplyr::rowwise() %>%
  dplyr::transmute(pa = sum({{ds}}) > 0) %>%  # mutate は他の行が残るのでダメ
  dplyr::filter(hub_master, .)
}
## df(input, hub_plus)からmessageを生成
make_message <- function(df){
  #   cnames <- colnames(df)
  df %>%
  #     magrittr::set_colnames(c("input", "hub_plus")) %>%
    dplyr::select(input, hub_plus) %>%
    dplyr::group_by(input) %>%
    dplyr::transmute(input, message=reduce(hub_plus, ~str_c(.x, "；", .y))) %>%
  #     magrittr::set_colnames(cnames) %>%
    distinct()
}
## df(all_name, hub_plus)からmessageを生成(入力の列名が上とは異なる)
make_message <- function(df){
  df %>%
    dplyr::select(all_name, hub_plus) %>%
    dplyr::group_by(all_name) %>%
    dplyr::transmute(all_name, message=reduce(hub_plus, ~str_c(.x, "；", .y))) %>%
    distinct()
}
## df(input, hub_plus)からmessageを生成
make_message3 <- function(df){
  cnames <- colnames(df)
  df %>%
    magrittr::select(1:2)
    magrittr::set_colnames("input", "hub_plus") %>%
    dplyr::select(input, hub_plus) %>%
    dplyr::group_by(input) %>%
    dplyr::transmute(input, message=reduce(hub_plus, ~str_c(.x, "；", .y))) %>%
    magrittr::set_colnames(cnames) %>%
    distinct()
}
## df(input, hub_plus)からmessageを生成，カタカナ部分の省略バージョン
make_message2 <- function(df){
  #   cnames <- colnames(df)
  df %>%
  #     magrittr::set_colnames("input", "hub_plus") %>%
    dplyr::select(input, hub_plus) %>%
    dplyr::group_by(input) %>%
    tidyr::separate(hub_plus, into=c("hub", "plus1", "plus2"), sep="-", fill="right") %>%
    dplyr::mutate(plus1=tidyr::replace_na(plus1, ""), plus2=tidyr::replace_na(plus2, "")) %>%
    dplyr::mutate(hub   = case_when( hub   == lag(hub)   ~ "", TRUE ~ hub)) %>%
    dplyr::mutate(plus1 = case_when( plus1 == lag(plus1) ~ "", TRUE ~ plus1)) %>%
    dplyr::mutate(hub_plus=str_c(hub, plus1, plus2)) %>%
    dplyr::transmute(input, message=reduce(hub_plus, ~str_c(.x, "；", .y))) %>%
  #     magrittr::set_colnames(cnames) %>%
    distinct()
}
## hub_nameが同じだが，idの組み合わせが異なるものに，識別用の記号をつける(下の関数の古いバージョン)
  # add_diff_letter_old <- function(hub, hub_master){
  #   # masterからidの組み合わせを作成
  #   hub_ids <- 
  #     hub_master %>%
  #     dplyr::filter(status=="！未統合") %>%
  #     dplyr::transmute(hub_plus=paste(Hub_name, lato_stricto, sep="-"), ids=paste(GL, SF, WF, YL, sep="-"), message)
  #   # 最後の結合用にidsを追加
  #   hub_res <- left_join(hub, hub_ids)
  #   # 識別記号
  #   diff <- 
  #     hub_res %>%
  #     dplyr::filter(status=="！未統合") %>%
  #     dplyr::distinct(hub_plus, ids) %>%
  #     dplyr::group_by(hub_plus) %>%
  #     dplyr::filter(n()>1) %>%
  #     dplyr::mutate(diff=1) %>%
  #     dplyr::mutate(diff=accumulate(diff, ~ .x + 1)) %>%
  #     dplyr::mutate(diff=letters[diff]) # アルファベットの場合(数字のときは不要)
  #   # 統合して出力
  #   hub_res %>%
  #     left_join(diff) %>%
  #     dplyr::mutate(hub_plus=paste(hub_plus, diff, sep="-")) %>%
  #     dplyr::mutate(hub_plus=str_replace_all(hub_plus, "-NA", "")) %>%
  #     dplyr::select(-ids, -diff)
  # }
## hub_nameが同じだが，idの組み合わせが異なるものに，識別用の記号をつける
add_diff_letter <- function(hub){
  # masterからidの組み合わせを作成
  hub_ids <- dplyr::mutate(hub,ids=paste(GL, SF, WF, YL, sep="_"))
  # 識別記号
  diff <- 
    hub_ids %>%
    dplyr::filter(status=="！未統合") %>%
    dplyr::select(hub_plus, ids) %>%
    dplyr::group_by(hub_plus) %>%
    dplyr::filter(n()>1) %>%
    dplyr::mutate(diff=1) %>%
    dplyr::mutate(diff=purrr::accumulate(diff, ~ .x + 1)) %>%
    dplyr::mutate(diff=letters[diff]) # アルファベットの場合(数字のときは不要)
  # 統合して出力
  hub_ids %>%
    dplyr::left_join(diff) %>%
    dplyr::mutate(hub_plus=paste(hub_plus, diff, sep="-")) %>%
    dplyr::mutate(hub_plus=stringr::str_replace_all(hub_plus, "-NA", "")) %>%
    dplyr::select(-ids, -diff)
}
## compose版 ちょっと遅くなるので，ボツ
  # arrange_message2 <- function(x){
  #   fncs <- compose(
  #     ~tibble::tibble(hub_plus=.x), 
  #     ~tidyr::separate(.x, hub_plus, into=c("hub", "plus"), sep="-", fill="right"), 
  #     ~dplyr::transmute(.x, 
  #         hub_plus = case_when( hub == lag(hub) ~ hub, TRUE ~ paste(hub, plus, sep="")), 
  #         hub_plus = reduce(hub_plus, ~str_c(.x, "/", .y))),
  #     ~dplyr::distinct(.x),
  #     .dir=c("forward")
  #   )
  #   # 個別の和名を分解
  #   x <-
  #     x %>%
  #     stringr::str_split("，|/") %>%
  #     purrr::map(sort) %>%
  #     purrr::map(unique)
  #   # tibbleにしてから，case_whenやreduceなどの処理
  #   x <-
  #     x %>% 
  #       purrr::map(fncs) %>%
  #       unlist()
  #   # 細かな修正
  #   x %>%
  #     `names<-`(NULL) %>%
  #     stringr::str_replace_all("NA", "")
  # }
## オリジナルのhubとjnからfull形式を作成
wamei_master2full <- function(hub_master, jn_master){
  full <- 
    hub_master %>%
    dplyr::mutate(Hub_name=stringr::str_replace_all(Hub_name, " ", "_")) %>%
    dplyr::mutate(all_name=stringr::str_replace_all(all_name, " ", "_")) %>%
    dplyr::mutate(hub_plus = hub2plus(Hub_name, lato_stricto)) %>% # hub_plus
    dplyr::select(-Hub_name, -lato_stricto) %>%
  #     add_diff_letter() %>%                                             # 識別記号の追加
    tidyr::pivot_longer(cols=GL:YL, names_to="source", values_to="ID", values_drop_na=TRUE) %>% # ID(CAPITAL)はjnとjoinするため，大文字のまま
    dplyr::right_join(jn_master) %>%
    dplyr::distinct()
  full <- 
    full %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename(sname_wo_author=scientific_name_without_author, sname_w_author=scientific_name_with_author) %>%
    dplyr::relocate(all_name, hub_plus)
  # 複数科に合致のall_name対応
  fml <- full2fml(full)
  full <- 
    full %>%
    dplyr::select(! starts_with("family")) %>%
    dplyr::left_join(fml)
}
## hub, jn, id, fmlからfull形式を作成
wamei_full <- function(hub, id, jn, fml=NULL){
  full <- 
    hub %>%
    dplyr::left_join(id) %>%
    dplyr::left_join(jn)
  if(! is.null(fml)) full <- dplyr::left_join(full, fml)
  full %>%
    dplyr::distinct()
}
## fullからhubを抽出
wamei_hub <- function(full){
  cols <- c("hub_plus", "all_name", "status", "message")
  full %>% 
    dplyr::select(starts_with(cols)) %>% 
    dplyr::distinct()
}
## fullからjnを抽出
wamei_jn <- function(full){
  cols  <- c("id", "source", "common", "another", "sname", "note")
  full %>% 
    dplyr::select(starts_with(cols)) %>% 
    dplyr::distinct()
}
## fullからidを抽出
wamei_id <- function(full){
  cols  <- c("id", "source", "hub_plus")
  full %>% 
    dplyr::select(starts_with(cols)) %>% 
    dplyr::distinct()
}
## fullからfamilyを抽出
wamei_fml <- function(full){
  cols <- c("all_name", "hub_plus", "family")
  full %>% 
    dplyr::select(starts_with(cols)) %>% 
    dplyr::distinct()
}
## fullからfamilyを作成(複数科に合致のall_nameにも対応済)
full2fml <- function(full){
  fml <-
    full %>%
    dplyr::select(all_name, family_id, family_name, family_name_jp) %>%
    dplyr::distinct() %>%
    dplyr::group_by(all_name)
  fml <- 
    fml %>%
    dplyr::filter(n()>1) %>%
    dplyr::arrange(all_name) %>%
    dplyr::mutate(family_id      = purrr::reduce(family_id,      paste, sep="；")) %>%
    dplyr::mutate(family_name    = purrr::reduce(family_name,    paste, sep="；")) %>%
    dplyr::mutate(family_name_jp = purrr::reduce(family_name_jp, paste, sep="；")) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(dplyr::filter(fml, n()==1))
}
## 各種データソースを抽出
gl <- function(x) dplyr::filter(x, source=="GL") %>% dplyr::distinct()
sf <- function(x) dplyr::filter(x, source=="SF") %>% dplyr::distinct()
wf <- function(x) dplyr::filter(x, source=="WF") %>% dplyr::distinct()
yl <- function(x) dplyr::filter(x, source=="YL") %>% dplyr::distinct()

## 各データソースの横長・横長の変換
wider  <- function(x, ...) tidyr::pivot_wider(x,  names_from=source, values_from=id, ...)
longer <- function(x, ...) tidyr::pivot_longer(x, names_to="source", values_to="id", cols=any_of(c("GL", "SF", "WF", "YL")), ...)
## 各データソース内でのIDの重複確認
check_dup_id <- function(x){
  wide_x <- tidyr::pivot_wider(x, names_from=source, values_from=id)
  wide_x_n <- tidyr::pivot_wider(x, names_from=source, values_from=id, values_fn=length)
  wide_x <- 
    tibble::as_tibble(wide_x_n>1) %>%
    dplyr::rowwise() %>%
    dplyr::transmute(tmp = sum(c_across(GL:WF), na.rm=TRUE)) %>%
    dplyr::bind_cols(wide_x) %>%
    dplyr::filter(tmp>0) %>%
    dplyr::select(-`NA`)
  wide_x <- 
    wide_x %>%
    dplyr::mutate(GL=GL %>% unlist() %>% stringr::str_c(collapse="；")) %>%
    dplyr::mutate(SF=SF %>% unlist() %>% stringr::str_c(collapse="；")) %>%
    dplyr::mutate(WF=WF %>% unlist() %>% stringr::str_c(collapse="；")) %>%
    dplyr::mutate(YL=YL %>% unlist() %>% stringr::str_c(collapse="；"))
}

## 列名の文字列置換
colnames_replace_all <- function(df, pattern, replacement) {
  cnames <- colnames(df)
  colnames(df) <- stringr::str_replace_all(cnames, pattern, replacement)
  df
}
## 1対1の対応確認
check_one2one <- function(df, col_1=NULL, col_2=NULL, keep_all=FALSE){
  # 行の抽出
  if( ! ncol(df)==2 ) df <- dplyr::select(df, all_of(col_a, col_b))
  c_names <- colnames(df)
  if(is.null(col_1)) col_1 <- c_names[1]
  if(is.null(col_2)) col_2 <- c_names[2]
  # グループ化して，回数を計算
  df <- 
    df %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data[[col_1]]) %>%
    dplyr::mutate(n_1=n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data[[col_2]]) %>%
    dplyr::mutate(n_2=n()) %>%
    dplyr::ungroup()
  # 重複の抽出・並べ替え
  if( ! keep_all ){
    df <- 
      df %>%
      dplyr::filter( n_1 > 1  |  n_2 > 1 ) %>%
      dplyr::arrange(.data[[col_1]], .data[[col_2]])
  }
  # 列名の整理
  c_names <- c(c_names, stringr::str_c("n_", c_names[1]), stringr::str_c("n_", c_names[2]))
  colnames(df) <- c_names
  df
}
check_one2one <- function(df){
  c_names <- colnames(df)
  colnames(df) <- c("col_1", "col_2")
  df <- 
    df %>%
    dplyr::distinct() %>%
    dplyr::left_join(df, by=c("col_1"="col_1")) %>%
    dplyr::left_join(df, by=c("col_2.y"="col_2")) %>%
    dplyr::mutate(diff=(col_2.x == col_2.y  &  col_1.x == col_1.y)) %>%
    dplyr::select(-any_of(c("col_2.y", "col_1.y")))
  colnames(df) <- c(c_names, "diff")
  df %>%
    dplyr::distinct() %>%
    dplyr::arrange(diff, c_names[1], c_names[2])
}
## 和名チェク(新版だが，ちょっと古い)
wamei_check0 <- function(x, full){
  # 該当データ
  res <- 
    tibble::tibble(input=x) %>%
    dplyr::left_join(dplyr::filter(full, another_name_id==0), by=c("input"="all_name")) %>%
    dplyr::select(-another_name_id)

  # 結果の区分用(n=1：OK or 該当なし，n>1：複数が一致)
  len <- 
    res %>%
    dplyr::distinct(input, hub_plus, source, id) %>%
    dplyr::group_by(input, source) %>%
    dplyr::mutate(n_matches=n()) %>%
    dplyr::group_by(input) %>%
    dplyr::mutate(n_matches=max(n_matches)) %>%
    distinct(input, hub_plus, n_matches)

  # OK or 該当なし
  res_ok <- 
    len %>%
    dplyr::filter(n_matches==1) %>%
    dplyr::left_join(res) %>%
    dplyr::group_by(input, source) %>%
    dplyr::arrange(input, source) %>%
    dplyr::mutate(
      id              = reduce(id             , ~str_c(.x, "；", .y)),
      sname_w_author  = reduce(sname_w_author , ~str_c(.x, "；", .y)),
      sname_wo_author = reduce(sname_wo_author, ~str_c(.x, "；", .y))) %>%
    dplyr::select(! c(note_1, note_2, message, another_name)) %>%
    dplyr::distinct() %>%
      # 同じ分類群に対して，データソースによって学名が異なる場合があるため，学名もwideにする
    tidyr::pivot_wider(names_from=source, values_from=c(id, common_name, starts_with("sname")), names_glue="{.value}_{source}") %>%
    dplyr::select(! ends_with("_NA")) %>%
    dplyr::mutate(n_matches = case_when( is.na(hub_plus) ~ 0, TRUE ~ 1))

  # 複数一致のinput：hub_plusの統合
  res_ng <- 
    len %>%
    dplyr::filter(n_matches>1) %>%
    make_message() %>%
    left_join(distinct(len, input, n_matches)) %>%
    dplyr::left_join(dplyr::select(res, -message, -hub_plus), by="input") %>% # もとのmessageとhubは不要，make_message2で作ったものに置き換える
    dplyr::distinct() %>%
    dplyr::rename(hub_plus=message)

  # 複数一致のinput(続き)：common_name, id, 和名, 学名等の処理
  # pivot_widerのvalues_fnを使って一気には解決出来なかったので，先に結合
  res_ng <- 
    res_ng %>%
    dplyr::group_by(input, source) %>%
    dplyr::arrange(input, source) %>%
    dplyr::select(input, hub_plus, n_matches, matches("family"), status, source, id, common_name, sname_wo_author, sname_w_author) %>%
    dplyr::mutate(
      common_name     = dplyr::case_when(common_name == lag(common_name) ~ "", TRUE ~ common_name),
      id              = purrr::reduce(id             , ~str_c(.x, "；", .y)),
      common_name     = purrr::reduce(common_name    , ~str_c(.x, "；", .y)),
      sname_w_author  = purrr::reduce(sname_w_author , ~str_c(.x, "；", .y)),
      sname_wo_author = purrr::reduce(sname_wo_author, ~str_c(.x, "；", .y)),
      common_name     = stringr::str_replace(common_name, "；$", "")) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(input:status, names_from=source, values_from=c(id, common_name, starts_with("sname")), names_glue="{.value}_{source}") %>%
    dplyr::mutate(hub_plus=arrange_hub_name(hub_plus))
  # 全体を統合
  res <- 
    dplyr::bind_rows(res_ok, res_ng) %>%
    dplyr::mutate(status=stringr::str_replace(status, "！未統合", "分類未統合")) %>%
    dplyr::mutate(status=stringr::str_replace(status, "確定", "")) %>%
    dplyr::ungroup() %>%
    relocate(input, hub_plus, n_matches, status)
  # 入力順に合わせる
  left_join(tibble(input=x), res)
}
## 和名チェク(新版だが，ちょっと古い)
wamei_check2 <- function(x, hub_master, jn_master){
  # 該当した数
  matches <- 
    tibble(input=x) %>%
    left_join(hub_master, by=c("input"="all_name")) %>%
    group_by(input) %>%
    tally(name="n_matches")
  # 全体のデータ
  res <- 
    wamei_master2full(hub_master, jn_master) %>%
    dplyr::filter(another_name_id==0) %>%
    dplyr::right_join(tibble(input=x), by=c("all_name"="input")) %>%
    dplyr::rename(input=all_name) %>%
    dplyr::select(-another_name_id, -note_1, -note_2)
  # OK or 該当なし
  res_ok <- dplyr::filter(matches, n_matches==1)
  if(nrow(res_ok) > 0){
    res_ok <- 
      res_ok %>%
      dplyr::left_join(res, by=c("input")) %>%
      dplyr::group_by(input, source) %>%
      dplyr::arrange(input, source) %>%
      dplyr::mutate(
        id              = reduce(id             , ~str_c(.x, "；", .y)),
        sname_w_author  = reduce(sname_w_author , ~str_c(.x, "；", .y)),
        sname_wo_author = reduce(sname_wo_author, ~str_c(.x, "；", .y))) %>%
      dplyr::select(! c(message, another_name)) %>%
      dplyr::distinct() %>%
        # 同じ分類群に対して，データソースによって学名が異なる場合があるため，学名もwideにする
      tidyr::pivot_wider(names_from=source, values_from=c(id, common_name, starts_with("sname")), names_glue="{.value}_{source}") %>%
      dplyr::select(! ends_with("_NA")) %>%
      dplyr::mutate(n_matches = case_when( is.na(hub_plus) ~ 0, TRUE ~ 1))
  }
  # 複数一致：hub_plusの統合
  res_ng <- dplyr::filter(matches, n_matches>1)
  if(nrow(res_ng) > 0){
    res_ng <- 
      res_ng %>%
      dplyr::left_join(dplyr::distinct(res, input, hub_plus), by=c("input")) %>%
      make_message() %>%
      dplyr::distinct() %>%
      dplyr::rename(hub_plus=message) %>%
      dplyr::left_join(dplyr::select(res, -hub_plus), by=c("input")) %>%
      dplyr::mutate(status = purrr::reduce(status, ~paste(.x, .y, sep=""))) %>%
      dplyr::left_join(matches)
  # 複数一致のinput(続き)：common_name, id, 和名, 学名等の処理
  # pivot_widerのvalues_fnを使って一気には解決出来なかったので，先に結合
  res_ng <- 
    res_ng %>%
    dplyr::group_by(input, source) %>%
    dplyr::arrange(input, source) %>%
    dplyr::select(input, hub_plus, n_matches, matches("family"), status, source, id, matches("name")) %>%
    dplyr::mutate(
      common_name     = dplyr::case_when(common_name == lag(common_name) ~ "", TRUE ~ common_name),
  #       id              = purrr::reduce(id             , ~paste(.x, .y, sep="；")),
  #       common_name     = purrr::reduce(common_name    , ~paste(.x, .y, sep="；")),
  #       sname_w_author  = purrr::reduce(sname_w_author , ~paste(.x, .y, sep="；")),
  #       sname_wo_author = purrr::reduce(sname_wo_author, ~paste(.x, .y, sep="；")),
      id              = purrr::reduce(id             , ~str_c(.x, "；", .y)),
      common_name     = purrr::reduce(common_name    , ~str_c(.x, "；", .y)),
      sname_w_author  = purrr::reduce(sname_w_author , ~str_c(.x, "；", .y)),
      sname_wo_author = purrr::reduce(sname_wo_author, ~str_c(.x, "；", .y)),
      common_name     = stringr::str_replace(common_name, "；$", "")) %>%
    dplyr::select(-another_name) %>%
    dplyr::distinct() %>%
  #     tidyr::pivot_wider(input:status, names_from=source, values_from=c(id, common_name, starts_with("sname")), names_glue="{.value}_{source}")
    tidyr::pivot_wider(input:status, names_from=source, values_from=c(id, common_name, starts_with("sname")), names_glue="{.value}_{source}") %>%
    dplyr::mutate(hub_plus=arrange_hub_name(hub_plus)) %>%
    dplyr::distinct()
  }
  # res_ng %>% arrange(input) %>%print(n=100)
  # res_ng %>% select(1:10) %>% arrange(input) %>%print(n=100)

  # 全体を統合
  res <- 
    dplyr::bind_rows(res_ok, res_ng) %>%
    dplyr::mutate(status=stringr::str_replace_all(status, "確定", "")) %>%
    dplyr::mutate(status=stringr::str_replace_all(status, "(！未統合)+", "分類未統合")) %>%
    dplyr::ungroup() %>%
    relocate(input, hub_plus, n_matches, status)
  # 入力順に合わせる
  res <- left_join(tibble(input=x), res)
  res
  # res %>% select(1:10) %>% arrange(input) %>%print(n=100)
}
## エクセルの機能置き換え(ちょっと古い)
wamei_check_excel <- function(
    x,                                         # string or vector     チェックする和名
    cl_file = "wamei_checklist_ver.1.10.xlsx", # checklist file      和名チェックリストのファイル
    cl_dir  = NULL,                            # checklist directory 上記ファイルのディレクトリ
    wamei_list  = "all",                       # out put list         出力する和名のリスト  c("all", "GL", "SF", "WF", "YL")
    sname       = FALSE,                       # scientific name     学名出力の有無，c(FALSE, "with_author", "without_author"),
    family_jp   = TRUE,
    family_id   = FALSE,
    family_sci  = FALSE,
    other_names = FALSE
  ){
  # Downlowd from https://www.gbif.jp/v2/activities/wamei_checklist.html
  # wamei_list: c("GL", "SF", "WF", "YL")
  # 
  # read data from excel file


  # cl_file = "wamei_checklist_ver.1.10.xlsx" # check list file      和名チェックリストのファイル
  # cl_dir  = "D:/matu/work/tmp/veg/"         # check list directory 上記ファイルのディレクトリ

  # ファイルの読み込み
  path <- stringr::str_c(cl_dir, cl_file)
  hub <-
    readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% 
    colnames_replace_all("[ /]", "_") %>%
    colnames_replace_all("[()]", "")
  jn <- 
    readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% 
    colnames_replace_all("[ /]", "_") %>% 
    colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる


  # x <- name$name
  # wamei_list = "all"                         # out put list         出力する和名のリスト
  # family_jp  = TRUE
  # family_id  = FALSE
  # family_sci = FALSE
  # sname      = FALSE                          # c(FALSE, "with_author", "without_author")


  # 学名出力の有無
  family_jp   <- if(family_jp)         "Family_name"             else NULL
  family_id   <- if(family_id)         "Family_ID"               else NULL
  family_sci  <- if(family_sci)        "Family_name"             else NULL
  wamei_list  <- if(wamei_list=="all") c("GL", "SF", "WF", "YL") else wamei_list
  sname <- 
    if(sname==FALSE){ NULL } else {
      switch(sname,
        "with_author"      = "with_author",
        "without_author"   = "without_author",
        "all"              = c("with_author", "without_author"),
        NULL
      )
    }

  out_cols <- c(family_jp, family_id, family_sci, wamei_list, sname)


  # 合致するか
  res <- 
    tibble(input=x) %>%
    dplyr::left_join(hub, by=c("input"="all_name"))

  # 該当なし：messageを表示
  no_match <- 
    res %>%
    dplyr::filter(is.na(Hub_name)) %>%
    dplyr::transmute(input, n_match=0, Hub_name, status, message="該当なし")

  # 合致した数
  res <- 
    res %>%
    dplyr::filter( !is.na(Hub_name) ) %>%
      # 以下の2行：1入力に2出力(エゾイチゴ問題)の問題対応(解決したら，group_by(input)でなく，group_by(input, message)を使う)
    dplyr::group_by(input) %>%
  # dplyr::group_by(input, message) %>%
    dplyr::summarise(n_match=n(), .groups="drop")

  # 2つ以上が合致：messageを表示
  need_check <- 
    res %>%
    dplyr::filter(n_match>1)  %>%
      # 以下の2行：エゾイチゴ問題対応(解決したら削除する)
    dplyr::left_join(dplyr::select(hub, all_name, Hub_name, status, message), by=c("input"="all_name")) %>%  # 解決後の削除対象
    dplyr::distinct(input, .keep_all=TRUE) %>%                                             # 解決後の削除対象
    dplyr::transmute(input, n_match=0, Hub_name, status, message=stringr::str_c("要確認，該当数：", n_match, "，", message))

  # 1つだけ合致
  res <- 
    res %>%
    dplyr::filter(n_match==1) %>%
    dplyr::transmute(input, n_match, all_name=input)

  # 各IDの統合 + 縦長へ
  res <- 
    res %>%
    dplyr::left_join(dplyr::select(hub, all_name, Hub_name, status, all_of(wamei_list))) %>%
    tidyr::pivot_longer(cols=all_of(wamei_list), names_to="source", values_to="ID", values_drop_na=TRUE) %>%
    dplyr::left_join(jn) %>%
    dplyr::select(-another_name, -note_1, -note_2) %>%
    dplyr::filter(another_name_ID==0) %>%
    dplyr::select(-another_name_ID) %>%
    dplyr::distinct(input, ID, .keep_all=TRUE) %>%
    tidyr::pivot_wider(
      names_from=source, 
      values_from=c(ID, common_name:scientific_name_without_author),
      names_glue = "{source}_{.value}",
      values_fill="-"
    ) %>%
    dplyr::mutate(message="ok")

  # 和名修正
  # replace_name <- 
  #   dplyr::filter(., input != common_name) %>%
  #   dplyr::mutate(message=stringr::str_c(input, "  ->  ", common_name))

  # ToDo?
  #   リスト形式で出力?  list(ok=ok, need_check=need_check, no_match=no_match)


  # 全部合わせて出力
  tibble(input=x) %>%
    dplyr::left_join(dplyr::bind_rows(res, need_check, no_match))

}
## 1つの和名に対して，複数の科名がないかチェック
  ## 1対1の関係になっているのかの確認にも使えそう
check_dup_famly <- function(df, name_col, fml_col){
  df %>%
    select({{name_col}}, {{fml_col}}) %>%
    distinct() %>%
    group_by({{name_col}}) %>%
    filter(n()>1) %>%
    arrange({{name_col}})
  # check_dup_famly(hub_master, all_name, Family_name_JP) %>% print(n=200)
}


## 以下は過去のコード
  # ## 新たな関数(初期バージョン)
  # wamei_check_old <- function(x, full){
  #   # 該当データ
  #   res <- 
  #     tibble::tibble(input=x) %>%
  #     dplyr::left_join(dplyr::filter(full, another_name_id==0), by=c("input"="all_name")) %>%
  #     dplyr::select(-another_name_id)
  # 
  #   # 結果の区分用(n=1：OK or 該当なし，n>1：複数が一致)
  #   len <- 
  #     res %>%
  #     dplyr::distinct(input, hub_plus, source, id) %>%
  #     dplyr::group_by(input, source) %>%
  #     dplyr::mutate(n_matches=n()) %>%
  #     dplyr::group_by(input) %>%
  #     dplyr::mutate(n_matches=max(n_matches)) %>%
  #     distinct(input, hub_plus, n_matches)
  # 
  #   # OK or 該当なし
  #   res_ok <- 
  #     len %>%
  #     dplyr::filter(n_matches==1) %>%
  #     dplyr::left_join(res) %>%
  #     dplyr::group_by(input, source) %>%
  #     dplyr::arrange(input, source) %>%
  #     dplyr::mutate(
  #       id              = reduce(id             , ~str_c(.x, "；", .y)),
  #       sname_w_author  = reduce(sname_w_author , ~str_c(.x, "；", .y)),
  #       sname_wo_author = reduce(sname_wo_author, ~str_c(.x, "；", .y))) %>%
  #     dplyr::select(! c(note_1, note_2, message, another_name)) %>%
  #     dplyr::distinct() %>%
  #       # 同じ分類群に対して，データソースによって学名が異なる場合があるため，学名もwideにする
  #     tidyr::pivot_wider(names_from=source, values_from=c(id, common_name, starts_with("sname")), names_glue="{.value}_{source}") %>%
  #     dplyr::select(! ends_with("_NA")) %>%
  #     dplyr::mutate(n_matches = case_when( is.na(hub_plus) ~ 0, TRUE ~ 1))
  # 
  #   # 複数一致のinput：hub_plusの統合
  #   res_ng <- 
  #     len %>%
  #     dplyr::filter(n_matches>1) %>%
  #     make_message() %>%
  #     left_join(distinct(len, input, n_matches)) %>%
  #     dplyr::left_join(dplyr::select(res, -message, -hub_plus), by="input") %>% # もとのmessageとhubは不要，make_message2で作ったものに置き換える
  #     dplyr::distinct() %>%
  #     dplyr::rename(hub_plus=message)
  # 
  #   # 複数一致のinput(続き)：common_name, id, 和名, 学名等の処理
  #   # pivot_widerのvalues_fnを使って一気には解決出来なかったので，先に結合
  #   res_ng <- 
  #     res_ng %>%
  #     dplyr::group_by(input, source) %>%
  #     dplyr::arrange(input, source) %>%
  #     dplyr::select(input, hub_plus, n_matches, matches("family"), status, source, id, common_name, sname_wo_author, sname_w_author) %>%
  #     dplyr::mutate(
  #       common_name     = dplyr::case_when(common_name == lag(common_name) ~ "", TRUE ~ common_name),
  #       id              = purrr::reduce(id             , ~str_c(.x, "；", .y)),
  #       common_name     = purrr::reduce(common_name    , ~str_c(.x, "；", .y)),
  #       sname_w_author  = purrr::reduce(sname_w_author , ~str_c(.x, "；", .y)),
  #       sname_wo_author = purrr::reduce(sname_wo_author, ~str_c(.x, "；", .y)),
  #       common_name     = stringr::str_replace(common_name, "；$", "")) %>%
  #     dplyr::distinct() %>%
  #     tidyr::pivot_wider(input:status, names_from=source, values_from=c(id, common_name, starts_with("sname")), names_glue="{.value}_{source}") %>%
  #     dplyr::mutate(hub_plus=arrange_hub_name(hub_plus))
  #   # 全体を統合
  #   res <- 
  #     dplyr::bind_rows(res_ok, res_ng) %>%
  #     dplyr::mutate(status=stringr::str_replace(status, "！未統合", "分類未統合")) %>%
  #     dplyr::mutate(status=stringr::str_replace(status, "確定", "")) %>%
  #     dplyr::ungroup() %>%
  #     relocate(input, hub_plus, n_matches, status)
  #   # 入力順に合わせる
  #   left_join(tibble(input=x), res)
  # }


  # tmp1 <-
  #   res %>%
  #     dplyr::left_join(dplyr::select(hub, all_name, Hub_name, all_of(wamei_list))) %>%
  #     tidyr::pivot_longer(cols=all_of(wamei_list), names_to="source", values_to="ID", values_drop_na=TRUE) %>%
  #     dplyr::left_join(jn) %>%
  #     dplyr::select(-Hub_name, -another_name, -note_1, -note_2) %>%
  #     dplyr::filter(another_name_ID==0) %>%
  #     dplyr::select(-another_name_ID) %>%
  #     dplyr::distinct(input, ID, .keep_all=TRUE) %>%
  #     tidyr::pivot_wider(
  #       id_cols=c(input, Family_ID: Family_name_JP), 
  #       names_from=source, 
  #       values_from=c(ID, common_name:scientific_name_without_author),
  #       names_glue = "{source}_{.value}",
  #       values_fill="-"
  #     )
  # 
  #   # 以下の2種が落ちるが，原因不明
  #   # c("オオセンボンヤリ", "ガーベラ")
  #   # another_name_IDの0が無い
  # tmp2 <- setdiff(res$input, tmp$input)
  # dplyr::filter(res, input %in% tmp)
  # dplyr::filter(hub, all_name %in% tmp)
  # dplyr::filter(hub, Hub_name %in% tmp)
  # dplyr::filter(jn, ID=="YL_17759")
  # 
  # 
  # tmp3 <- 
  #   dplyr::select(jn, ID, another_name_ID) %>%
  #   dplyr::mutate(n=1) %>%
  #   tidyr::pivot_wider(
  #     names_from=another_name_ID,
  #     values_fn = length,
  #     values_from=n
  #   ) %>%
  #   dplyr::filter(is.na(`0`)) %>%
  #   .$ID
  # 
  # dplyr::filter(jn, ID %in% tmp3)
  #   # A tibble: 12 x 11
  #   #    ID       common_name            another_name         another_name_ID
  #   #  1 SF_00127 オクノヤシャゼンマイ   オクノヤシャゼンマイ               1
  #   #  2 SF_00131 オオバヤシャゼンマイ   オクタマゼンマイ                   1
  #   #  3 SF_00323 ニシノコハチジョウシダ コハチジョウシダ                   1
  #   #  4 WF_01542 リュウキュウスゲ       リュウキュウスゲ                   1
  #   #  5 WF_01902 スズメノテッポウ       スズメノテッポウ                   1
  #   #  6 WF_02219 ムラサキノキビ         ムラサキノキビ                     1
  #   #  7 WF_03825 アメリカナナカマド     アメリカナナカマド                 1
  #   #  8 WF_04287 ヒタチヤナギ           ヒタチヤナギ                       1
  #   #  9 YL_11456 カンラン               カンラン                           4
  #   # 10 YL_17759 ハナグルマ             ハナグルマ                         1
  #   # 11 YL_17759 ハナグルマ             オオセンボンヤリ                   2
  #   # 12 YL_17759 ハナグルマ             ガーベラ                           3




  # res %>%
  #   dplyr::left_join(dplyr::select(jn, 1:5), by=c("input"="common_name")) %>%
  #   dplyr::filter(is.na(Family_ID))
  # 
  # system.time(
  # tmp %>%
  #   dplyr::group_by(common_name, source) %>%
  #   dplyr::filter(n()==1)
  # )
  # system.time(
  # tmp %>%
  #   dplyr::group_by(common_name, source) %>%
  #   dplyr::filter(n()==1) %>%
  #   tidyr::pivot_wider(
  #      id_cols=common_name , 
  #      names_from=source, 
  #      values_from=c(ID, scientific_name_with_author),
  #      names_glue = "{source}_{.value}"
  #    )
  # )
  # tmp %>%
  #   tidyr::pivot_wider(
  #      id_cols=ID, 
  #      names_from=c(source),
  #      values_from=c(common_name, scientific_name_with_author),
  #      names_glue = "{source}_{.value}"
  #    )
  # 
  # jn %>%
  #   dplyr::filter(another_name_ID==0) %>%
  #   dplyr::select(-another_name, -another_name_ID, -note_1, -note_2, -starts_with("Family"), -scientific_name_without_author) %>%
  #   dplyr::mutate(ID2=ID) %>%
  #   tidyr::separate(ID2, into=c("source", NA))   %>%
  #   tidyr::pivot_wider(
  #      id_cols=ID , 
  #      names_from=source,
  #      values_from=c(common_name, scientific_name_with_author),
  #      names_glue = "{source}_{.value}"
  #    )




  # 1つだけ合致
  #   res <- 
  #     res %>%
  #     dplyr::filter(n_match==1) %>%
  #     dplyr::select(input) %>%
  #     dplyr::left_join(dplyr::select(hub, all_name, Hub_name, GL:YL), by=c("input"="all_name")) %>%
  #     colnames_replace_all(wamei_list, "ID") %>%     # join の by で変数を使えないため
  #     dplyr::left_join(jn, by="ID") %>%
  #     colnames_replace_all("^ID$", wamei_list) %>%   # 上の変更をもとにもどす
  #     dplyr::filter(another_name_ID==0) %>%
  #     dplyr::select(input, Hub_name, all_of(wamei_list), common_name, starts_with(sci_name)) %>%
  #     dplyr::distinct() %T>%
  #     # 和名修正
  #       { replace_name <<- 
  #           dplyr::filter(., input != common_name) %>%
  #           dplyr::mutate(message=stringr::str_c(input, "  ->  ", common_name))
  #       } %>%
  #     dplyr::filter(input == common_name) %>%
  #     dplyr::mutate(message="ok") %>%
  #   # 全部合わせて出力
  #   dplyr::bind_rows(replace_name, need_check, no_match)
  # 
  # 
  # 
  #   res <- 
  #     tibble(input=x) %>%
  #     dplyr::left_join(hub, by=c("input"="all_name")) %>%
  #  # 以下の2行：1入力に2出力の問題対応(解決したら，group_by(input)でなく，group_by(input, message)を使う)
  #  #    dplyr::group_by(input, message) %>%
  #     dplyr::group_by(input) %>%
  #     dplyr::summarise(n_match=n(), .groups="drop") %T>%
  #     # 要確認
  #       { need_check <<- 
  #           dplyr::filter(., n_match>1)  %>%
  #  # 以下の2行：エゾイチゴ問題対応，解決したら削除する
  #           dplyr::left_join(dplyr::select(hub, all_name, message), by=c("input"="all_name")) %>%
  #           dplyr::distinct() %>%
  #           dplyr::transmute(input, message=stringr::str_c("要確認，該当数：", n_match, "，", message))
  #       } %>%
  #     dplyr::filter(n_match==1) %>%
  #     dplyr::select(input) %>%
  #     dplyr::left_join(dplyr::select(hub, all_name, Hub_name, GL:YL), by=c("input"="all_name")) %T>%
  #     # 該当なし
  #       { no_match <<- 
  #           dplyr::filter(., is.na(Hub_name)) %>%
  #           dplyr::transmute(input, message="該当なし")
  #       } %>%
  #     colnames_replace_all(wamei_list, "ID") %>%     # join の by で変数を使えないため
  #     dplyr::left_join(jn, by="ID") %>%
  #     colnames_replace_all("^ID$", wamei_list) %>%   # 上の変更をもとにもどす
  #     dplyr::filter(another_name_ID==0) %>%
  #     dplyr::select(input, Hub_name, all_of(wamei_list), common_name, starts_with(sci_name)) %>%
  #     dplyr::distinct() %T>%
  #     # 和名修正
  #       { replace_name <<- 
  #           dplyr::filter(., input != common_name) %>%
  #           dplyr::mutate(message=stringr::str_c(input, "  ->  ", common_name))
  #       } %>%
  #     dplyr::filter(input == common_name) %>%
  #     dplyr::mutate(message="ok") %>%
  #   # 全部合わせて出力
  #   dplyr::bind_rows(replace_name, need_check, no_match)

  # jnを横長形式に変換
  #   jn <-
  #     jn %>%
  #     dplyr::filter(another_name_ID==0) %>%
  #     dplyr::mutate(ID2=ID) %>%
  #     tidyr::separate(ID2, into=c("source", NA))   %>%
  #     dplyr::distinct(common_name, source, .keep_all=TRUE) %>%
  #     tidyr::pivot_wider(
  #        id_cols=Family_ID:common_name, 
  #        names_from=source, 
  #        values_from=c(ID, scientific_name_with_author, scientific_name_without_author),
  #        names_glue = "{source}_{.value}"
  #      )

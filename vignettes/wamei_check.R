## 新しいバージョン
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)
options(encoding="UTF-8")
source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")


  # version
  # packageVersion("tidyverse")
  # packageVersion("readxl")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub_master <-
  readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>%
  dplyr::rename_with(~str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~str_replace_all(., "[()]", ""))
jn_master <- 
  readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% 
  dplyr::rename_with(~str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~str_replace_all(., "[()]", "")) %>%
  fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる


  # another_name_IDに0がないものへの対応
no_id_0 <- c("SF_00131", "SF_00323", "WF_01542", "WF_02219", "WF_04287", "SF_00127","WF_01902","WF_03825","YL_11456","YL_17759")
jn_master$another_name_ID[jn_master$ID %in% no_id_0 & jn_master$another_name_ID != 0] <- 0

  # シベリアカラマツ問題への対応
hub_master$Hub_name[hub_master$Hub_name=="シベリアカラマツ" & hub_master$Family_name_JP=="マツ"]       <- "シベリアカラマツ(マツ科)"
hub_master$Hub_name[hub_master$Hub_name=="シベリアカラマツ" & hub_master$Family_name_JP=="キンポウゲ"] <- "シベリアカラマツ(キンポウゲ科)"
jn_master$Family_name_JP[jn_master$Family_name_JP=="ツルボラン"] <- "ワスレグサ"

  # hubの種名
name <- 
  c(hub_master$all_name, hub_master$Hub_name, jn_master$common_name, jn_master$another_name) %>%
  map(str_split, "/") %>%
  unlist() %>% unique() %>% sort() %>%
  c("だみーの和名", .) %>%
  tibble(name=.)

sp <- c("アオイモドキ","アオヤギソウ","アカミノイヌツゲ","アメリカヒイラギ",
  "アヤメ","イモノキ","イワグルマ","オオイワウチワ","オオボシシダ","オオムラサキ",
  "オキナグサ","オキナワクジャク","カイノキ","カスミソウ","カンザクラ","カンゾウ",
  "ガンピ","カンラン","キンギョモ","キンレンカ",
  "クジャクソウ","グミモドキ","ケウツギ","コウライシダ","コガネヤナギ","ゴサイバ",
  "コバノクスドイゲ","サンゴバナ","サンショウモドキ","シコクショウマ",
  "ジシバリ","シノノメソウ","シベリアカラマツ","ショウキラン","スイショウ",
  "スズメノヒエ","スズラン","セイタカスズムシソウ","タカノツメ","タチガシワ",
  "タビラコ","タマゴノキ","タマナ","タマボウキ","チシャノキ","チョウジザクラ",
  "チョウセンカラマツ","トキワナズナ","トネリコシダ","ナギ",
  "ナンジャモンジャ","ノニンジン","ハグマノキ","ハチス","ハナグルマ","ヒメアマナ",
  "ヒメスズムシソウ","ヒメデンダ","ヒモカズラ","ビロードウツギ",
  "ブンタン","ベニチョウジ","ホウライヒメワラビ","ボダイジュモドキ","ホトケノザ",
  "ミズスギナ","ミズトラノオ","ミズナ","ミミガタシダ","ミヤマゼキショウ",
  "ムギナデシコ","ムク","モクタチバナ","ヤグルマソウ","ヤマグワ","ヤマゴボウ",
  "ユウガオ","ユキワリソウ","リョクチク","リョクトウ","レンギョウ")

x <- c("だみー", "ススキ", "ハリガネワラビ", "オミナエシ", "コナスビ", "カナビキソウ", 
  "ヤイトバナ", "チガヤ", "キジムシロ", "ハエドクソウ", "キツネノマゴ", "シロヨメナ", 
  "オオフジシダ", "コマツナギ", "アイヌタチツボスミレ", "シベリアカラマツ", "アオイモドキ")

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")
wamei_check(x, hub_master, jn_master)
wamei_check(x, hub_master, jn_master, wide=FALSE)
wamei_check(x, hub_master, jn_master,             ds=c(GL, SF, WF))
wamei_check(x, hub_master, jn_master, wide=FALSE, ds=c(GL, SF, WF))



system.time( ck1 <- wamei_check(x, hub_master, jn_master, wide=FALSE) )
system.time( ck2 <- wamei_check(x=sp, hub_master, jn_master) )
system.time( ck3  <- wamei_check(x=name$name, hub_master, jn_master, wide=FALSE) )
system.time( ck4 <- wamei_check(x, hub_master, jn_master) )
system.time( ck5 <- wamei_check(x=name$name, hub_master, jn_master) )

wamei_check_excel2(x=x, hub_master, jn_master)
wamei_check_excel2(x=c("ススキ", sp), hub_master, jn_master)


  # editing now






  ## 完了


## エクセルの機能置き換え
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)
options(encoding="UTF-8")
source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

  # ファイルの読み込み
cl_file <- "d:/wamei_checklist_ver.1.10.xlsx"  # 和名チェックリストのファイル
path <- stringr::str_c(cl_file)
hub <- readxl::read_xlsx(path, sheet="Hub_data",  col_types="text") %>%
  colnames_replace_all("[ /]", "_") %>%
   colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>%
  colnames_replace_all("[ /]", "_") %>%
  colnames_replace_all("[()]", "")

  # シベリアカラマツ・ツルボラン科の対応
hub$Hub_name[hub$Hub_name=="シベリアカラマツ" & hub$Family_name_JP=="マツ"]       <- "シベリアカラマツ(マツ科)"
hub$Hub_name[hub$Hub_name=="シベリアカラマツ" & hub$Family_name_JP=="キンポウゲ"] <- "シベリアカラマツ(キンポウゲ科)"
jn$Family_name_JP[jn$Family_name_JP=="ツルボラン"] <- "ワスレグサ"

  # 種名リストを作成
name <- 
  c(hub$all_name, hub$Hub_name, jn$common_name, jn$another_name) %>%
  map(str_split, "/") %>%
  unlist() %>% unique() %>% sort() %>%
  c("だみーの和名", .) %>%
  tibble(name=.)
name
  # write_tsv(name, "d:/name.txt")

  # wamei_check_excel2(x=name$name, hub=hub, jn=jn)              # 横長(エクセルと同じ形式)
  # wamei_check_excel2(x=name$name, hub=hub, jn=jn, wide=FALSE)  # 縦長

  # 別の種名リスト
x <- c("だみー", "ススキ", "ハリガネワラビ", "オミナエシ", "コナスビ", "カナビキソウ", 
  "ヤイトバナ", "チガヤ", "キジムシロ", "ハエドクソウ", "キツネノマゴ", "シロヨメナ", 
  "オオフジシダ", "コマツナギ", "アイヌタチツボスミレ", "シベリアカラマツ", "アオイモドキ")

  # wamei_check_excel2(x=x, hub=hub, jn=jn)
  # wamei_check_excel2(x=x, hub=hub, jn=jn, wide=FALSE)


  # エクセルとRの結果の比較
r_result <- wamei_check_excel2(x=name$name, hub=hub, jn=jn)

  # エクセル(name_convert_sheet_ver. 1.10.xlsx)を使った結果
path <- "D:/matu/work/tmp/veg/excel_result.txt"
ex_result <-readr::read_tsv(path) %>% colnames_replace_all(" ", "_")


ex_result
r_result


  # NAなどの整理
r_result[is.na(r_result)]    <- "-"
ex_result[ex_result=="#N/A"] <- "-"
ex_result[ex_result=="！"]   <- "-"

  # 列名を合わせる
ex_result <- 
  ex_result %>%
  colnames_replace_all("↓↓貼り付け部分↓↓", "input") %>%
  colnames_replace_all("該当和名数", "n_match") %>%
  colnames_replace_all("Hub和名", "Hub_name")

r_result <-
  r_result %>%
  #   dplyr::select( -ends_with("_scientific_name_without_author") ) %>%
  #   dplyr::select( -Family_name, -message) %>%
  colnames_replace_all("_scientific_name_with_author", "学名") %>%
  colnames_replace_all("_common_name",                 "和名代表") %>%
  colnames_replace_all("_ID",                          "") %>%
  colnames_replace_all("Family_name_JP", "科") %>%
  colnames_replace_all("Family", "科ID") %>%
  dplyr::select(all_of(colnames(ex_result)))


  # Rとエクセルで違うところ
as_tibble(r_result != ex_result) %>% sum()
as_tibble(r_result != ex_result) %>% apply(2, sum)

dif <- as_tibble(r_result != ex_result) %>% apply(1, sum)
r_result[dif,]  %>% print(n=100)
ex_result[dif,] %>% print(n=100)

r_result  %>% readr::write_tsv("D:/r.txt")
ex_result %>% readr::write_tsv("D:/ex.txt")





## エクセルバージョンとRとバージョンの比較(たぶん不要)
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)
source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub_master <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% 
  colnames_replace_all("[()]", "")
jn_master <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>%
   colnames_replace_all("[()]", "") %>% fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

  # シベリアカラマツ問題への対応
hub_master$Hub_name[hub_master$Hub_name=="シベリアカラマツ" &
  hub_master$Family_name_JP=="マツ"]       <- "シベリアカラマツ(マツ科)"
hub_master$Hub_name[hub_master$Hub_name=="シベリアカラマツ" &
  hub_master$Family_name_JP=="キンポウゲ"] <- "シベリアカラマツ(キンポウゲ科)"
jn_master$Family_name_JP[jn_master$Family_name_JP=="ツルボラン"] <- "ワスレグサ"

full <- wamei_master2full(hub_master, jn_master)
full <- filter(full, !is.na(all_name))  # IDがhubに無いもの対策

  # hubの種名
name <- 
  c(hub_master$all_name, hub_master$Hub_name, jn_master$common_name, jn_master$another_name) %>%
  map(str_split, "/") %>%
  unlist() %>% unique() %>% sort() %>%
  c("だみーの和名", .) %>%
  tibble(name=.) %>% 
  readr::write_tsv("D:/matu/work/tmp/veg/name.txt")

  # 内容の確認(カタカナ以外を含むもの)
name %>% dplyr::arrange(name) %>% dplyr::filter( str_detect(name, "\\p{Han}|\\p{Hiragana}|[A-z]") ) %>% print(n=100)

  # エクセル(name_convert_sheet_ver. 1.10.xlsx)を使った結果
path <- "D:/matu/work/tmp/veg/excel_result.txt"
ex_result <-readr::read_tsv(path) %>% colnames_replace_all(" ", "_")


source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")
  # R関数でのチェック
dir <- "D:/matu/work/tmp/veg/"
r_result <- 
  name$name %>%
  wamei_check_exel(cl_dir=dir)

  # NAなどの整理
r_result$n_match[is.na(r_result$n_match)] <- 0
r_result[is.na(r_result)]    <- "-"
ex_result[ex_result=="#N/A"] <- "-"
ex_result[ex_result=="！"]   <- "-"

  # 列名を合わせる
ex_result <- 
  ex_result %>%
  colnames_replace_all("↓↓貼り付け部分↓↓", "input") %>%
  colnames_replace_all("該当和名数", "n_match") %>%
  colnames_replace_all("Hub和名", "Hub_name")

r_result <-
  r_result %>%
  dplyr::select( -ends_with("_scientific_name_without_author") ) %>%
  dplyr::select( -Family_name, -message) %>%
  colnames_replace_all("_scientific_name_with_author", "学名") %>%
  colnames_replace_all("_common_name",                 "和名代表") %>%
  colnames_replace_all("_ID",                          "") %>%
  colnames_replace_all("Family_name_JP", "科") %>%
  colnames_replace_all("Family", "科ID") %>%
  dplyr::select(all_of(colnames(ex_result)))


  # Rとエクセルで違うところ
as_tibble(r_result != ex_result) %>%
  apply(2, sum) %>%
  sum()
  #   apply(1, sum) %>%
  #   sum()

r_result %>%
  filter(科=="ツルボラン")

r_result2  <- r_result  %>% pivot_longer(cols=科ID:YL学名, names_to="item", values_to="val")
ex_result2 <- ex_result %>% pivot_longer(cols=科ID:YL学名, names_to="item", values_to="val")
sum(r_result2 != ex_result2)
tmp_id <- unique(r_result2[r_result2 != ex_result2]) %>% .[str_detect(., "_")]

  # Rとエクセルで結果の異なる入力
diff_result <- 
  r_result2[r_result2$val != ex_result2$val, ] %>%
  select(input) %>%
  distinct() %>%
  print(n=nrow(.))


  # 同一Hub_name(広義・狭義含む)だが，IDの組み合わせ表記が異なる
hub_id_diff <- 
  hub %>% 
  transmute(hub_plus=paste(Hub_name, lato_stricto, sep="_"), GL, SF, WF, YL, status) %>%
  distinct() %>%
  group_by(hub_plus) %>%
  filter(n()>1) %>%
  pivot_longer(cols=GL:YL, names_to="source", values_to="ID",  values_drop_na=TRUE) %>%
  group_by(hub_plus, ID) %>%
  filter(n()>1) %>%
  print(n=nrow(.)) %>%  # 一旦出力
  ungroup() %>%
  select(hub_plus) %>%
  distinct() %>%
  separate(hub_plus, into=c("Hub_name", NA), sep="-") %>%
  left_join(hub) %>%
  print(n=nrow(.))

hub_id_diff <- 
  hub_id_diff %>%
  select(input=all_name)


  # 同一Hub_name(広義・狭義なし)だが，IDの組み合わせ表記が異なる
hub_id_diff2 <- 
  hub %>% 
  transmute(Hub_name, ids=paste(GL, SF, WF, YL, sep="-"), status) %>%
  mutate(ids=str_replace_all(ids, "NA", "........")) %>%
  distinct() %>%
  group_by(Hub_name) %>%
  filter(n()>1) %>%
  ungroup() %>%
  filter(status != "！未統合") %>%
  left_join(hub)

hub_id_diff2 <- 
  hub_id_diff2 %>%
  select(input=all_name)


  # 各Hub_name，各IDでanother_name_ID==0の無いやつ
no_0_id <- 
  jn %>%
  transmute(ID, another_name_ID, n=1) %>%
  distinct() %>%
  pivot_wider(names_from=another_name_ID, values_from=n, values_fill=0, names_glue="ID_{another_name_ID}") %>%
  filter(ID_0==0)
no_0_id <- 
  hub %>%
  select(all_name, GL:YL) %>%
  pivot_longer(cols=GL:YL, names_to="source", values_to="ID",  values_drop_na=TRUE) %>%
  left_join(select(no_0_id, ID), .)


diff_input <- 
  diff_result %>%
  left_join(r_result) %>%
  filter(科 != "ツルボラン") %>%
  filter(! input %in% hub_id_diff2$input) %>%
  filter(! input %in% hub_id_diff$input) %>%
  filter(! input %in% no_0_id$all_name) %>%
  select(! ends_with("学名")) %>%
  arrange(科, GL, WF, YL) %>%
  print(n=nrow(.))

diff_input %>%
  select(all_name=input) %>%
  slice(c(1, 6, 8, 9, 10)) %>%
  left_join(hub) %>%
  select(Hub_name) %>%
  left_join(hub) %>%
  select(! starts_with("Family")) %>%
  arrange(Hub_name, lato_stricto ) %>%
  print(n=nrow(.))

 # 1 ハナトリカブト    -   -    -    -   -    -    -          -     -          -         
 # 6 トマティーヨ     423  ナス   -    -   WF_06358 -    -          -     オオブドウホオズキ  -         
 # 8 ケエゾノシモツケソウ 206  バラ   -    -   WF_03517 YL_08809 -          -     エゾノシモツケソウ  エゾノシモツケソウ
 # 9 コウライカノコ    206  バラ   -    -   WF_03517 YL_08809 -          -     エゾノシモツケソウ  エゾノシモツケソウ
 # 10 コキア        360  ヒユ   -    -   WF_05409 YL_12978 -          -     イソホウキギ     ホウキギ     

list(r_result, ex_result) %>% map(filter, input=="ハナトリカブト")
  # ハナトリカブト：WF_02447とYL_06808は別物?
  #   Hub_dataでは同じ組み合わせになっているが，JN_datasetでは別の学名

list(r_result, ex_result) %>% map(filter, input=="トマティーヨ")
list(r_result, ex_result) %>% map(filter, input=="ケエゾノシモツケソウ")
list(r_result, ex_result) %>% map(filter, input=="コウライカノコ")
list(r_result, ex_result) %>% map(filter, input=="コキア")
  # トマティーヨケエゾノシモツケソウ, コウライカノコ, コキア
  #   原因：未統合の別のものが，vlookupで引っかかっている
  #   対策：別物っぽいものは，別のHub_nameにする


 # 以下は，Hub_nameとIDの関係の修正で治りそうなところ
 # 
 # 2 ホクセンナガハグサ  166  イネ   GL_03438 -   -    YL_06284 ホクセンイチゴツナギ -     -          ホクセンナガハグサ
 # 3 タイワンセンダン   306  センダン -    -   -    YL_11781 -          -     -          タイワンセンダン 
 # 4 トキワセンダン    306  センダン -    -   -    YL_11781 -          -     -          タイワンセンダン 
 # 5 オウチ        306  センダン -    -   WF_04846 YL_11777 -          -     センダン       センダン     
 # 7 タイワンクロヅル   231  ニシキギ -    -   -    YL_10172 -          -     -          タイワンクロヅル 
 # 11 オオナギラン     124  ラン   -    -   WF_00760 YL_03092 -          -     オオナギラン     オオナギラン   
 # 12 オニナギラン     124  ラン   -    -   WF_00760 YL_03092 -          -     オオナギラン     オオナギラン   
 # 13 アリサンヨウラクラン 124  ラン   -    -   WF_00927 -    -          -     クスクスヨウラクラン -         
 # 14 ナメラサギソウ    124  ラン   GL_02000 -   -    YL_03266 リュウキュウサギソウ -     -          ナメラサギソウ  

a <- 
  diff_input %>%
  select(input) %>%
  left_join(r_result) %>%
  #   select(ends_with("学名")) %>%
  print(n=nrow(.))
b <- 
  diff_input %>%
  select(input) %>%
  left_join(ex_result) %>%
  #   select(ends_with("学名")) %>%
  print(n=nrow(.))
sum(a!=b)


## messageにアルファベットの入っているもの(山ノ内さんに送付済み)
hub_master %>%
  filter(str_detect(message, "[A-z]")) %>%
  select(all_name, Hub_name, message) %>%
  slice(5:20) %>%
  .$all_name %>%
  unique()
  # モンパヤンバルクルマバナ, ヤンバルクルマバナ, ヤンバルツルハッカ, 
  # ダキバナンブアザミ, ダキバヒメアザミ, トネアザミ, ナンブアザミ

## messageの自動生成(山ノ内さんに送付済み)
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

colnames_replace_all <- function(df, pattern, replacement) {
  cnames <- colnames(df)
  colnames(df) <- stringr::str_replace_all(cnames, pattern, replacement)
  df
}
hub2plus <- function(Hub_name, lato_stricto){
  hub_name <- Hub_name %>% str_split("/")
  purrr::map2(hub_name, lato_stricto, paste, sep="-", collapse="/") %>%
    stringr::str_replace_all("-NA", "")
}
make_message <- function(df){
  df %>%
    dplyr::select(all_name, hub_plus) %>%
    dplyr::group_by(all_name) %>%
    dplyr::transmute(all_name, message=reduce(hub_plus, ~str_c(.x, "；", .y))) %>%
    distinct()
}
arrange_hub_name <- function(x){
  # 個別の和名を分解
  x <-
    x %>%
    stringr::str_split("，|；|/") %>%
    purrr::map(sort) %>%
    purrr::map(unique)
  # tibbleにしてから，case_whenやreduceなどの処理
  x <-
    x %>%
    purrr::map(~tibble::tibble(hub_plus=.)) %>%
    purrr::map(~tidyr::separate( ., hub_plus, into=c("hub", "plus"), sep="-", fill="right")) %>%
    purrr::map(~dplyr::transmute(., 
      hub_plus = case_when( hub == lag(hub) ~ plus, TRUE ~ paste(hub, plus, sep="")), 
      hub_plus = reduce(hub_plus, ~str_c(.x, "/", .y))    )) %>%
    purrr::map(distinct) %>%
    unlist()
  # 細かな修正
  x %>%
    `names<-`(NULL) %>%
    stringr::str_replace_all("NA", "")
}

path <- "D:/wamei_checklist_ver.1.10.xlsx"
hub_master <- 
  readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>%
  colnames_replace_all("[ /]", "_") %>% 
  colnames_replace_all("[()]", "")

hub <- dplyr::transmute(hub_master, all_name, hub_plus = hub2plus(Hub_name, lato_stricto))
auto_message <- 
  hub %>%
  dplyr::group_by(all_name) %>%
  dplyr::filter(n() > 1) %>%
  make_message() %>%
  dplyr::mutate(message = arrange_hub_name(message)) %>%
  dplyr::mutate(message = stringr::str_replace(message, "^/", "")) %>%
  dplyr::mutate(message = stringr::str_replace_all(message, "/+", "/")) %>%
  left_join(hub) %>%
  dplyr::mutate(hub_plus = reduce(hub_plus, paste, sep="；")) %>%
  dplyr::mutate(hub_plus = stringr::str_replace_all(hub_plus, "-", "")) %>%
  dplyr::arrange(all_name) %>%
  dplyr::distinct() %>%
  readr::write_tsv("d:/auto_message.txt")



## イソノキと少し追加したデータの抽出(tiny)
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

  # 使用する場合
    # hub_master <- readr::read_tsv("D:/matu/work/tmp/veg/hub_tiny.txt")
    # jn_master  <- readr::read_tsv("D:/matu/work/tmp/veg/jn_tiny.txt")

jname <- c("イソノキ", "コゴメバクロウメモドキ", "クマノミズキ" ,"ハナシノブ", "リョウメンシダ")

hub_tiny <- 
  bind_rows(filter(hub, Hub_name %in% jname), filter(hub, all_name %in% jname)) %>%
  distinct()

ids <- 
  hub_tiny %>%
  select(GL:YL) %>%
  pivot_longer(cols=GL:YL, names_to="source", values_to="ID", values_drop_na=TRUE) %>%
  distinct() %>%
  .$ID

jn_tiny <- 
  jn %>%
  filter(ID %in% ids) %>%
  distinct()

hub_tiny  %>% print() %>% readr::write_tsv("D:/matu/work/tmp/veg/hub_tiny.txt")
jn_tiny   %>% print() %>% readr::write_tsv("D:/matu/work/tmp/veg/jn_tiny.txt")


## DBの構成変更用のデータ抽出(small)
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

  # 使用する場合
    # hub_master <- readr::read_tsv("D:/matu/work/tmp/veg/hub_small.txt")
    # jn_master  <- readr::read_tsv("D:/matu/work/tmp/veg/jn_small.txt")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

  # hubの種名
hub_name <-   hub %>%  dplyr::select(name=all_name) %>%  distinct()
  # jnの種名
jn_name <-   c(jn$common_name, jn$another_name) %>%  tibble(name=.) %>%  distinct()

  # 動作確認用の種名
name <- 
  dplyr::union(hub_name, jn_name) %>%
  dplyr::arrange(name) %>%
  bind_rows(tibble(name="ダミーの種名"), .) %>%
  readr::write_tsv("D:/matu/work/tmp/veg/name.txt")

  # エクセル(name_convert_sheet_ver. 1.10.xlsx)を使った結果
path <- "D:/matu/work/tmp/veg/excel_result.txt"
ex_result <-readr::read_tsv(path) %>% colnames_replace_all(" ", "_")


  # R関数でのチェック
dir <- "D:/matu/work/tmp/veg/"
r_result <- 
  name$name %>%
  wamei_check(cl_dir=dir)

  # NAなどの整理
r_result$n_match[is.na(r_result$n_match)] <- 0
r_result[is.na(r_result)]    <- "-"
ex_result[ex_result=="#N/A"] <- "-"
ex_result[ex_result=="！"]   <- "-"

  # 列名を合わせる
ex_result <- 
  ex_result %>%
  colnames_replace_all("↓↓貼り付け部分↓↓", "input") %>%
  colnames_replace_all("該当和名数", "n_match") %>%
  colnames_replace_all("Hub和名", "Hub_name")

r_result <-
  r_result %>%
  dplyr::select( -ends_with("_scientific_name_without_author") ) %>%
  dplyr::select( -Family_name, -message) %>%
  colnames_replace_all("_scientific_name_with_author", "学名") %>%
  colnames_replace_all("_common_name",                 "和名代表") %>%
  colnames_replace_all("_ID",                          "") %>%
  colnames_replace_all("Family_name_JP", "科") %>%
  colnames_replace_all("Family", "科ID") %>%
  dplyr::select(any_of(colnames(ex_result)))

  # Rとエクセルで結果の同じやつから適当に抽出
same_row <- 
  as_tibble(r_result == ex_result) %>%
  apply(1, sum)==18 # <16
  #   apply(1, sum)<16
  # sum(same_row); sum(!same_row)
  # r_result[!same_row, ] %>% arrange(Hub_name) %>% select(!matches("学名|和名")) %>% print(n=nrow(.)) 

small_data <- 
  r_result[same_row, ] %>%
  slice(3501:3600) %>%
  select(input, GL:YL) %>%
  pivot_longer(cols=GL:YL, names_to="source", values_to="ID") %>%
  filter(ID != "-") %>%
  distinct(input, ID)

small_1 <- 
  small_data %>%
  left_join(hub, by=c("input"="all_name")) %>%
  select(-ID) %>%
  distinct() %>%
  rename(all_name=input)

small_2 <- 
  small_data %>%
  left_join(hub, by=c("input"="Hub_name")) %>%
  select(-ID) %>%
  distinct() %>%
  rename(Hub_name=input) %>%
  filter(!is.na(all_name))

small_3 <- 
  hub %>%
  filter(GL %in% unique(small_data$ID) | SF %in% unique(small_data$ID) | WF %in% unique(small_data$ID) | YL %in% unique(small_data$ID))

hub_small <- 
  bind_rows(small_1, small_2, small_3) %>%
  distinct()
jn_small <- 
  jn %>%
  filter(ID %in% unique(c(hub_small$GL, hub_small$SF, hub_small$WF, hub_small$YL))) %>%
  distinct()

hub_small %>%
  filter(status!="確定")

hub_small %>% arrange(Hub_name)    %>% print(n=nrow(.))
jn_small  %>% arrange(common_name) %>% print(n=200)

hub_small %>% readr::write_tsv("D:/matu/work/tmp/veg/hub_small.txt")
jn_small  %>% readr::write_tsv("D:/matu/work/tmp/veg/jn_small.txt")

## 1つのall_nameが複数の科にあるやつのチェック(問題なしなので，送付せず)
  # 1つのall_nameが複数の科にあるもの
dup_fml <- 
  check_dup_famly(hub_master, all_name, Family_name_JP) %>%
  select(hub_master, all_name, hub_plus, Family_name_JP) %>%
  
  left_join(full, by="all_name") %>%
  select(all_name, hub_plus, Family_name_JP) %>%
  arrange(all_name) %>%
  distinct()

  # 科が異なったら，別のHub_nameになっているか?
  #   たぶんOK
dup_fml %>% 
  left_join(select(hub_master, all_name, Hub_name, Family_name_JP)) %>%
  distinct() %>%
  print() %>%
  select(-Family_name_JP) %>%
  print()

## センダン問題，問題か?(報告せず)
  # 学名が同一かどうかで分けているっぽい
  #   作業場，分けているだけのような感じ
hub_master %>%
  filter(str_detect(all_name, "センダン$")) %>%
  print(n=100)
jn_master %>%
  filter(ID %in% c("YL_11782", "GL_06103", "WF_04846", "YL_11777", "YL_11781")) %>%
  print(n=100)

## 雑多なエラー(報告済み)

b <- c("サクゴクテツカエデ", "ヒメイヌノヒゲ", "ヤンバルツルハッカ広義")
jn_master %>% filter(common_name %in% b | another_name %in% b) %>% select(1:6)
hub_master %>% filter(str_detect(Hub_name, b[3])) %>% select(1:3)
hub_master %>% filter(str_detect(Hub_name, b[1])) %>% select(1:3)
  # ヒメイヌノヒゲ：jnのanother_nameにあるけど，Hubのall_nameにはない
  # サクゴクテツカエデ：「サイ」が「サク」になっている
  # ヤンパヤンバルクルマバナ/ヤンバルツルハッカ広義：「広義」が入っている

setdiff(jn_master$ID, c(hub_master$YL, hub_master$GL, hub_master$SF, hub_master$WF))
  # jnにはあるが，HubにはないID(大きな問題はない?)
  #    逆は無いので，大丈夫
  # GL_03957
  # GL_03959
  # GL_07814
  # GL_08924
  # WF_02954
  # WF_03043
  # WF_03796
  # WF_04251
  # YL_00169
  # YL_01215
  # YL_03720
  # YL_07241
  # YL_07242
  # YL_07818

## 広義/狭義の忘れ(問題なしなので，送付せず)
  # 
hub_master %>%
  filter(str_detect(message, "義") & is.na(lato_stricto)) %>%
  select(all_name, Hub_name, lato_stricto, message) %>%
  mutate(message=str_replace_all(message, "要検討（|）", "")) %>%
  separate(Hub_name, into=c("hub1", "hub2"), sep="/", fill="right") %>%
  separate(message, into=c(letters[1:5]), sep="/", fill="right") %>%
  mutate(hub2=replace_na(hub2, "_"), a=replace_na(a, ""), b=replace_na(b, ""), c=replace_na(c, ""), d=replace_na(d, ""), e=replace_na(e, "")) %>%  # hub2「_」はa-eと一致させないため
  arrange(hub1) %>%
  filter(hub1 != a & hub1 != b & hub1 != c & hub1 != d & hub1 != e  &  hub2 != a & hub2 != b & hub2 != c & hub2 != d & hub2 != e) %>%
  distinct() %>%
  print(n=nrow(.))
  # 他の情報は省略して，distinct(unique)しているので，複数行に渡るものがあります．
  # 例えば，キツネノマゴは2行あって，それぞれが広義，狭義が抜けていると思います．
  #    all_name               hub1                   hub2     lato_stricto message1                   message2             message3 
  #    <chr>                  <chr>                  <chr>    <chr>        <chr>                      <chr>                <chr>  
  #  1 イヌガヤ               イヌガヤ               _        <NA>         イヌガヤ広義               狭義                 ""     
  #  2 アマギザサ             イブキザサ             _        <NA>         イブキザサ広義             狭義                 ""     
  #  3 イブキザサ             イブキザサ             _        <NA>         イブキザサ広義             狭義                 ""     
  #  4 ツボイザサ             イブキザサ             _        <NA>         イブキザサ広義             狭義                 ""     
  #  5 ウラゲヒメアザミ       ウラゲヒメアザミ       _        <NA>         ウラゲアザミ               ナンブアザミ広義     "狭義"   #  ウラゲ「ヒメ」?
  #  6 オクヤマワラビ         オクヤマワラビ         _        <NA>         オクヤマワラビ広義         狭義                 ""     
  #  7 カキバカンコノキ       カキバカンコノキ       _        <NA>         カキバカンコノキ広義       狭義                 ""     
  #  8 カキバノカンコノキ     カキバカンコノキ       _        <NA>         カキバカンコノキ広義       狭義                 ""     
  #  9 セイロンカンコノキ     カキバカンコノキ       _        <NA>         カキバカンコノキ広義       狭義                 ""     
  # 10 カタバミ               カタバミ               _        <NA>         カタバミ広義               狭義                 ""     
  # 11 タチカタバミ           カタバミ               _        <NA>         カタバミ広義               狭義                 ""     
  # 12 キツネノマゴ           キツネノマゴ           _        <NA>         キツネノマゴ広義           狭義                 ""     
  # 13 クルマバナ             クルマバナ             _        <NA>         クルマバナ広義             狭義                 ""     
  # 14 クロヌマハリイ         クロヌマハリイ         _        <NA>         クロヌマハイリ広義         狭義                 ""     
  # 15 コバノチョウセンエノキ コバノチョウセンエノキ _        <NA>         コバノチョウセンエノキ広義 狭義                 ""     
  # 16 コバノトンボソウ       コバノトンボソウ       _        <NA>         コバノトンボソウ広義       狭義                 ""     
  # 17 サトウダイコン         サトウダイコン         テンサイ <NA>         サトウダイコン狭義         テンサイ広義         "狭義"   # 以前送ったもので解決済み?
  # 18 シオカゼテンツキ       シオカゼテンツキ       _        <NA>         シオカゼテンツキ広義       狭義                 ""     
  # 19 シバテンツキ           シオカゼテンツキ       _        <NA>         シオカゼテンツキ広義       狭義                 ""     
  # 20 ハマテンツキ           シオカゼテンツキ       _        <NA>         シオカゼテンツキ広義       狭義                 ""     
  # 21 タカネシバスゲ         タカネシバスゲ         _        <NA>         タカネシバスゲ広義         狭義                 ""     
  # 22 ダケスゲ               ダケスゲ               _        <NA>         ダケスゲ広義               狭義                 ""     
  # 23 チョウセンノギク       チョウセンノギク       _        <NA>         イワギク広義               チョウセンノギク広義 "狭義" 
  # 24 トリアシショウマ       トリアシショウマ       _        <NA>         トリアシショウマ広義       狭義                 ""     
  # 25 イソスゲ               ヒゲスゲ               _        <NA>         ヒゲスゲ広義               狭義                 ""     
  # 26 オオヒゲスゲ           ヒゲスゲ               _        <NA>         ヒゲスゲ広義               狭義                 ""     
  # 27 ヒゲスゲ               ヒゲスゲ               _        <NA>         ヒゲスゲ広義               狭義                 ""     
  # 28 ビランジ               ビランジ               _        <NA>         ビランジ広義               狭義                 ""     
  # 29 ヒロハノハネガヤ       ヒロハノハネガヤ       _        <NA>         ヒロハノハネガヤ広義       狭義                 ""     

## no_named関係
  # _(アンダーバー)と (スペース)の不統一，「_GL0X」の忘れ?

hub        %>% filter(str_detect(hub_plus, "named") | str_detect(all_name, "named")) %>% arrange(GL)
hub_master %>% filter(str_detect(Hub_name, "named") | str_detect(all_name, "named")) %>% arrange(GL)
hub_master %>% filter(str_detect(Hub_name, "_| ") | str_detect(all_name, "_| ")) %>% arrange(GL)
  # A tibble: 11 x 12
  #    all_name             Hub_name                      lato_stricto Family_ID Family_name      Family_name_JP GL       SF    WF       YL       status message                
  #    <chr>                <chr>                         <chr>        <chr>     <chr>            <chr>          <chr>    <chr> <chr>    <chr>    <chr>  <chr>                  
  #  1 no_named_GL01        no_named_GL01                 <NA>         101       Potamogetonaceae ヒルムシロ     GL_01555 <NA>  <NA>     <NA>     確定   <NA>                   
  #  2 no_named_GL02        ムカゴサイシンモドキ/no named <NA>         124       Orchidaceae      ラン           GL_02085 <NA>  WF_00924 YL_03378 確定   <NA>                   
  #  3 ムカゴサイシンモドキ ムカゴサイシンモドキ/no named <NA>         124       Orchidaceae      ラン           GL_02085 <NA>  WF_00924 YL_03378 確定   <NA>                   
  #  4 no_named_GL03        ミチノクサナギイチゴ/no named <NA>         206       Rosaceae         バラ           GL_04858 <NA>  WF_03668 YL_09238 確定   <NA>                   
  #  5 ミチノクサナギイチゴ ミチノクサナギイチゴ/no named <NA>         206       Rosaceae         バラ           GL_04858 <NA>  WF_03668 YL_09238 確定   <NA>                   
  #  6 no_named_GL04        ヤエガワカンバ/no named       狭義         221       Betulaceae       カバノキ       GL_05226 <NA>  <NA>     YL_09932 確定   <NA>                   
  #  7 コオノオレ           ヤエガワカンバ/no named       狭義         221       Betulaceae       カバノキ       GL_05226 <NA>  <NA>     YL_09932 確定   ヤエガワカンバ広義/狭義
  #  8 ヤエガワカンバ       ヤエガワカンバ/no named       狭義         221       Betulaceae       カバノキ       GL_05226 <NA>  <NA>     YL_09932 確定   ヤエガワカンバ広義/狭義
  #  9 no_named_GL05        no_named_GL05                 <NA>         221       Betulaceae       カバノキ       GL_05242 <NA>  <NA>     <NA>     確定   <NA>                   
  # 10 no_named_GL06        no_named_GL06                 <NA>         416       Gentianaceae     リンドウ       GL_07412 <NA>  <NA>     <NA>     確定   <NA>                   
  # 11 no_named_GL07        no_named_GL07                 <NA>         466       Asteraceae       キク           GL_08808 <NA>  <NA>     <NA>     確定   <NA>   

## 確定に要検討, all_name-message, status-hub_name
hub <- tibble::rownames_to_column(hub, "row_num")

  # 確定に要検討がある
er1 <- 
  hub %>%
  distinct(status, message, .keep_all=TRUE) %>%
  filter((status=="確定" &  str_detect(message, "要検討")) | (status=="！未確定" &  ! str_detect(message, "要検討"))) %>%
  mutate(er1=1) %>%
  print(n=nrow(.))

  # all_nameに対して複数のmessageがある(連絡済み)
er2 <- 
  hub %>%
  distinct(all_name, message, .keep_all=TRUE) %>%
  arrange(all_name) %>%
  group_by(all_name) %>%
  filter(n()>1) %>%
  mutate(er2=1) %>%
  print(n=nrow(.))

  # hub_plusに対して，確定・未統合の両方がある(all_nameベースで考えると問題はなさそうなので，山ノ内さんには伝えず)
er3 <- 
  hub %>%
  distinct(hub_plus, status, .keep_all=TRUE) %>%
  arrange(hub_plus) %>%
  group_by(hub_plus) %>%
  mutate(er3=1) %>%
  filter(n()>1) %>%
  print(n=nrow(.))


  # 全体を結合
er1 %>%
  full_join(er2) %>%
  full_join(er3) %>%
  print(n=nrow(.))

  # 以下はその他の問題(とりあえず不要)，山ノ内さんには送っていない

  # シロヨメナ問題
  #   all_nameのシロヨメナに，複数のHub_nameがある
  #   これは，OK
  #   つまり，1つの名前のものが，複数の分類群を指していることがある．
  # hub %>%
  #   distinct(all_name, hub_plus) %>%
  #   group_by(all_name) %>%
  #   tally() %>%
  #   filter(n>1)

  # 以下はその他の問題(とりあえず不要)，山ノ内さんには送っていない
  # hub_master %>%
  #   filter(status=="！未統合") %>%
  #   transmute(hub_plus=paste(Hub_name, lato_stricto, sep="-"), message) %>%
  #   distinct() %>%
  #   group_by(hub_plus) %>%
  #   tally() %>%
  #   arrange(desc(n)) %>%
  #   filter(n>1)
  # hub_master %>% filter(str_detect(message, "類"))
  # hub_master %>% filter(Hub_name=="ハマヤブマオ")
  # hub_master %>% filter(Hub_name=="ナンブアザミ")
  # hub_master %>% filter(Hub_name=="アマミトンボ")
  # hub_master %>% filter(all_name=="ヤクシマトンボ")

## another_name_idの抜け(飛び)がないか?
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(magrittr)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる


jn_an_id <- 
  jn %>%
  transmute(ID, common_name, another_name_ID, val=1) %>%
  pivot_wider(id_cols=, names_from=another_name_ID, values_from=val,  values_fill=0,  names_glue="id_{another_name_ID}",values_fn=length)

jn_an_id %>%
  filter(id_0>1 | id_1>1 | id_2>1 | id_3>1 | id_4>1 | id_5>1 | id_6>1 | id_7>1 | id_8>1 | id_9>1 | id_10>1) %>%
  print(n=nrow(.))

tibble(
   jn_an_id$ID, jn_an_id$common_name, 
   d1=jn_an_id[ 3] - jn_an_id[ 4], 
   d2=jn_an_id[ 4] - jn_an_id[ 5],
   d3=jn_an_id[ 5] - jn_an_id[ 6],
   d4=jn_an_id[ 6] - jn_an_id[ 7],
   d5=jn_an_id[ 7] - jn_an_id[ 8],
   d6=jn_an_id[ 8] - jn_an_id[ 9],
   d7=jn_an_id[ 9] - jn_an_id[10],
   d8=jn_an_id[10] - jn_an_id[11],
   d9=jn_an_id[11] - jn_an_id[12],
  d10=jn_an_id[12] - jn_an_id[13]
  ) %>%
  filter(d1<0 | d2<0 | d3<0 | d4<0 | d5<0 | d6<0 | d7<0 | d8<0 | d9<0)

  # 他は既に報告済み
  # 
  # another_name_IDの「2」がない
  #  jn_an_id$ID     jn_an_id$common_name
  #  1 GL_02255      タイワンハマオモト
  #  2 GL_02300      トウギボウシ
  #  3 GL_04777      オガサワラカジイチゴ
  #  4 GL_08106      エゾナミキ

## all_nameとmessageの関係
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")

  # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # 
  # 機械的にmessageを作成
  # 
  # # # # # # # # # # # # # # # # # # # # # # # # # # 

  # 複数あるall_nameのhub_plusの取り出し
msg <- 
  hub %>%
  transmute(all_name, hub_plus=paste(Hub_name, lato_stricto, sep="-")) %>%
  distinct() %>%
  group_by(all_name) %>%
  filter(n()>1) %>%
  arrange(all_name, hub_plus)

  # 同じHub_nameに対して，狭義/広義の場合の2つ目以降のHub_nameを空欄に
msg <- 
  msg %>%
  separate(hub_plus, into=c("hub", "plus"), sep="-") %>%
  group_by(all_name) %>%
  mutate(hub=case_when( hub==lag(hub) ~ "", TRUE ~ hub)) %>%
  transmute(all_name, hub_plus=str_c(hub, plus))


  # all_name内で，文字列の結合
msg <- 
  msg %>%
  transmute(all_name, message_new=reduce(hub_plus, ~str_c(.x, "/", .y))) %>%
  distinct() %>%
  mutate(message_new=str_replace_all(message_new, "NA", "")) %>%
  print(n=50)

  # 機械的にall_nameとHub_nameからmessageを作成
  #   そもそものHub_nameが「A/B」のように複数の和名からなっている場合は，
  #     A狭義/広義などの同じ和名の省略がうまくできていない．
  #     別途作業が必要Hub_name内の区分とHub_nameの区切りの両方とも「/」を使っているのが問題?
  #   狭義/広義であっても，A狭義/A広義とすれば問題なし?
msg %>% readr::write_tsv("d:/message_new.txt")

  # messageのがall_nameと1対1になっているのか確認
  #    -> なっていない，というか1対1である必要はない!
msg %>% check_one2one() %>% filter(diff==FALSE)


  # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # 
  # hubデータでのチェック
  # 
  # # # # # # # # # # # # # # # # # # # # # # # # # # 

  # messageのがall_nameと1対1になっているのか確認
  #    -> なっていない，というか1対1である必要はない!
msg <- 
  distinct(hub, all_name, message) %>% na.omit() %>% 
  check_one2one() %>% filter(diff==FALSE) %>%
  arrange(message)
msg

## 学名のチェック(各データIDと学名の対応)
  # Hub_nameと学名とは1対1対応にはならない
  # データソースごとに採用している学名が異なるため
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)
library(magrittr)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

hub_plus_id <- 
  hub %>%
  select(! starts_with("Family")) %>%
  filter(status=="確定") %>% select(-status) %>%
  mutate(hub_plus=paste(Hub_name, lato_stricto, sep="-"), message) %>% select(-Hub_name, -lato_stricto) %>%
  pivot_longer(cols=GL:YL, names_to="source", values_to="ID", values_drop_na=TRUE) %>%
  select(hub_plus, ID) %>%
  separate(ID, into=c("source", "no")) %>%
  distinct()

jn_sn_id_no_author   <- distinct(jn, sci_name=scientific_name_without_author, ID)
jn_sn_id_with_author <- distinct(jn, sci_name=scientific_name_with_author, ID)

  # 学名なし
jn_sn_id_no_author    %>%
  separate(ID, into=c("source", "no")) %>%
  split(.$source) %>%
  map(right_join, hub_plus_id) %>%
  map(distinct, hub_plus, sci_name) %>%
  map(filter, sci_name != "－") %>% 
  map(filter, hub_plus != "－") %>% 
  map(na.omit) %>%
  map(check_one2one) %>%
  map(filter, diff==FALSE)

  # 学名あり
jn_sn_id_with_author    %>%
  separate(ID, into=c("source", "no")) %>%
  split(.$source) %>%
  map(right_join, hub_plus_id) %>%
  map(distinct, hub_plus, sci_name) %>%
  map(filter, sci_name != "－") %>% 
  map(filter, hub_plus != "－") %>% 
  map(na.omit) %>%
  map(check_one2one) %>%
  map(filter, diff==FALSE)

  # 学名の有無で違いは無し
  #
  # $GL
  #   sci_name                               hub_plus
  # 1 Hemerocallis dumortieri var. esculenta ゼンテイカ_NA
  # 2 Hemerocallis dumortieri var. esculenta キキョウラン_狭義
  # 3 Pieris japonica subsp. japonica        アセビ_狭義1
  # 4 Pieris japonica subsp. japonica        アセビ_狭義
  #
  # $SF
  # # A tibble: 0 x 3
  # # ... with 3 variables: sci_name <chr>, hub_plus <chr>, diff <lgl>
  #
  # $WF
  #   sci_name                               hub_plus
  # 1 Hemerocallis dumortieri var. esculenta ゼンテイカ_NA
  # 2 Hemerocallis dumortieri var. esculenta キキョウラン_狭義
  # 3 Pleioblastus argenteostriatus 'Okina'  オキナタケ/オキナダケ_NA
  # 4 Pleioblastus argenteostriatus 'Okina'  オキナタケ/オキナダケ_狭義
  # 5 Cerasus × yedoensis 'Somei-yoshino'    ソメイヨシノ/染井吉野_狭義
  # 6 Cerasus × yedoensis 'Somei-yoshino'    ソメイヨシノ/染井吉野_NA
  # 7 Beta vulgaris var. altissima           サトウダイコン/テンサイ_NA
  # 8 Beta vulgaris var. altissima           サトウダイコン/テンサイ_狭義
  #
  # $YL
  #    sci_name                                      hub_plus
  #  1 Dianella ensifolia f. racemulifera            ゼンテイカ_NA
  #  2 Dianella ensifolia f. racemulifera            キキョウラン_狭義
  #  3 Pleioblastus argenteostriatus 'Okina'         オキナタケ/オキナダケ_NA
  #  4 Pleioblastus argenteostriatus 'Okina'         オキナタケ/オキナダケ_狭義
  #  5 Cerasus × yedoensis 'Somei-yoshino'           ソメイヨシノ/染井吉野_狭義
  #  6 Cerasus × yedoensis 'Somei-yoshino'           ソメイヨシノ/染井吉野_NA
  #  7 Impatiens furcillata                          チョウセンハガクレツリフネ_NA
  #  8 Impatiens furcillata                          ヤマツリフネソウ_NA
  #  9 Camellia wabisuke var. campanulata f. bicolor コチョウワビスケ_NA
  # 10 Camellia wabisuke var. campanulata f. bicolor ワビスケ_NA
  # 11 Pieris japonica subsp. japonica               アセビ_狭義1
  # 12 Pieris japonica subsp. japonica               アセビ_狭義

## 1対1の対応確認(Hub_nameと各データソースID)
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)
library(magrittr)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
  fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

hub_diff <- 
  hub %>%
  transmute(hub_plus=paste(Hub_name, lato_stricto, sep="-"), status, GL, SF, WF, YL) %>%
  pivot_longer(GL:YL, names_to="source", values_to="ID", values_drop_na=TRUE) %>%
  distinct(hub_plus, ID, .keep_all=TRUE) %>%
  #   filter(status != "確定") %>%
  arrange(source) %>%
  split(.$source) %>%
  map(select, -source, -status) %>%
  map(check_one2one) %T>%
  {
    source_names <- names(.)
    source_nrow  <- map(., nrow)
    source_df <<- tibble(source=rep(source_names, source_nrow))
  } %>%
  bind_rows() %>%
  bind_cols(source_df) %>%
  filter(diff==FALSE)

hub_diff %>%
  print(n=nrow(.))

  # 確定のみ
  #    hub_plus                     ID
  #  1 ゼンテイカ_NA                GL_02219
  #  2 キキョウラン_狭義            GL_02219
  #  3 アセビ_狭義1                 GL_06957
  #  4 アセビ_狭義                  GL_06957
  #  5 ゼンテイカ_NA                WF_01061
  #  6 キキョウラン_狭義            WF_01061
  #  7 オキナタケ/オキナダケ_NA     WF_01845
  #  8 オキナタケ/オキナダケ_狭義   WF_01845
  #  9 ソメイヨシノ/染井吉野_狭義   WF_03716
  # 10 ソメイヨシノ/染井吉野_NA     WF_03716
  # 11 サトウダイコン/テンサイ_NA   WF_05375
  # 12 サトウダイコン/テンサイ_狭義 WF_05375
  # 13 ゼンテイカ_NA                YL_03698
  # 14 キキョウラン_狭義            YL_03698
  # 15 オキナタケ/オキナダケ_NA     YL_06211
  # 16 オキナタケ/オキナダケ_狭義   YL_06211
  # 17 ソメイヨシノ/染井吉野_狭義   YL_08733
  # 18 ソメイヨシノ/染井吉野_NA     YL_08733
  # 19 アセビ_狭義1                 YL_13898
  # 20 アセビ_狭義                  YL_13898

  # ！未統合も含む
  #    hub_plus                                            ID
  #  1 オオフジシダ_NA                                     GL_00298
  #  2 キシュウシダ_NA                                     GL_00298
  #  3 ゼンテイカ_NA                                       GL_02219
  #  4 キキョウラン_狭義                                   GL_02219
  #  5 ハライヌノヒゲ/ユキイヌノヒゲ_NA                    GL_02446
  #  6 ハライヌノヒゲ/ユキイヌノヒゲ_NA                    GL_02460
  #  7 ハライヌノヒゲ/ユキイヌノヒゲ_NA                    GL_02428
  #  8 ジンボソウ/セイタカヌカボシソウ/ナスヌカボシソウ_NA GL_02498
  #  9 ジンボソウ/セイタカヌカボシソウ/ナスヌカボシソウ_NA GL_02499
  # 10 アオツヅラフジ/ホウザンツヅラフジ_NA                GL_03686
  # 11 アオツヅラフジ/ホウザンツヅラフジ_NA                GL_03685
  # 12 ハチジョウイチゴ×カジイチゴ/ヒメシマカジイチゴ_NA  GL_04847
  # 13 ハチジョウイチゴ×カジイチゴ/ヒメシマカジイチゴ_NA  GL_04850
  # 14 オオハシバミ/ハシバミ_NA                            GL_05253
  # 15 オオハシバミ/ハシバミ_狭義                          GL_05253
  # 16 オオハシバミ/ハシバミ_NA                            GL_05254
  # 17 タイワンスベリヒユ_NA                               GL_06551
  # 18 マルバケツメクサ_NA                                 GL_06551
  # 19 マルバケヅメグサ_NA                                 GL_06551
  # 20 アセビ_狭義1                                        GL_06957
  # 21 アセビ_狭義                                         GL_06957
  # 22 ヒメアカボシタツナミ/ヤクシマナミキ_NA              GL_08099
  # 23 ヒメアカボシタツナミ/ヤクシマナミキ_NA              GL_08087
  # 24 ヤクシマシソバタツナミ/ヤクシマナミキ_NA            GL_08087
  # 25 アイフジシダ_NA                                     SF_00272
  # 26 オオフジシダ_NA                                     SF_00272
  # 27 オオフジシダ_NA                                     SF_00270
  # 28 キシュウシダ_NA                                     SF_00270
  # 29 アキザキナギラン/オオナギラン_NA                    WF_00759
  # 30 アキザキナギラン/オオナギラン_NA                    WF_00760
  # 31 ゼンテイカ_NA                                       WF_01061
  # 32 キキョウラン_狭義                                   WF_01061
  # 33 オキナタケ/オキナダケ_NA                            WF_01845
  # 34 オキナタケ/オキナダケ_狭義                          WF_01845
  # 35 ソメイヨシノ/染井吉野_狭義                          WF_03716
  # 36 ソメイヨシノ/染井吉野_NA                            WF_03716
  # 37 オオハシバミ/ハシバミ_NA                            WF_03973
  # 38 オオハシバミ/ハシバミ_狭義                          WF_03973
  # 39 イソホウキギ_広義                                   WF_05409
  # 40 サトウダイコン/テンサイ_NA                          WF_05375
  # 41 サトウダイコン/テンサイ_狭義                        WF_05375
  # 42 ホウキギ_広義                                       WF_05409
  # 43 タイワンスベリヒユ_NA                               WF_05471
  # 44 マルバケツメクサ_NA                                 WF_05471
  # 45 タイワンスベリヒユ_NA                               WF_05470
  # 46 マルバケヅメグサ_NA                                 WF_05471
  # 47 ヒメアカボシタツナミ/ヤクシマナミキ_NA              WF_06746
  # 48 ヤクシマシソバタツナミ/ヤクシマナミキ_NA            WF_06746
  # 49 アイフジシダ_NA                                     YL_00477
  # 50 オオフジシダ_NA                                     YL_00477
  # 51 オオフジシダ_NA                                     YL_00475
  # 52 キシュウシダ_NA                                     YL_00475
  # 53 アキザキナギラン/オオナギラン_NA                    YL_03083
  # 54 アキザキナギラン/オオナギラン_NA                    YL_03092
  # 55 ゼンテイカ_NA                                       YL_03698
  # 56 キキョウラン_狭義                                   YL_03698
  # 57 オキナタケ/オキナダケ_NA                            YL_06211
  # 58 オキナタケ/オキナダケ_狭義                          YL_06211
  # 59 ホクセンイチゴツナギ/ホクセンナガハグサ_NA          YL_06307
  # 60 ホクセンイチゴツナギ/ホクセンナガハグサ_NA          YL_06284
  # 61 アオツヅラフジ/ホウザンツヅラフジ_NA                YL_06726
  # 62 アオツヅラフジ/ホウザンツヅラフジ_NA                YL_06725
  # 63 コマツナギ/トウコマツナギ_NA                        YL_08131
  # 64 コマツナギ/トウコマツナギ_NA                        YL_08144
  # 65 イトザクラ_NA                                       YL_08654
  # 66 イトザクラ_NA                                       YL_08655
  # 67 ソメイヨシノ/染井吉野_狭義                          YL_08733
  # 68 ソメイヨシノ/染井吉野_NA                            YL_08733
  # 69 オオハシバミ/ハシバミ_NA                            YL_09973
  # 70 オオハシバミ/ハシバミ_狭義                          YL_09973
  # 71 オオハシバミ/ハシバミ_NA                            YL_09974
  # 72 センダン/タイワンセンダン_NA                        YL_11777
  # 73 センダン/タイワンセンダン_NA                        YL_11782
  # 74 センダン/タイワンセンダン_NA                        YL_11781
  # 75 オランダナデシコ/カーネーション_NA                  YL_12705
  # 76 オランダナデシコ/カーネーション_NA                  YL_12733
  # 77 ビランジ_NA                                         YL_12846
  # 78 ビランジ_NA                                         YL_12848
  # 79 イソホウキギ_広義                                   YL_12978
  # 80 ホウキギ_広義                                       YL_12978
  # 81 タイワンスベリヒユ_NA                               YL_13127
  # 82 マルバケツメクサ_NA                                 YL_13127
  # 83 マルバケヅメグサ_NA                                 YL_13127
  # 84 アセビ_狭義1                                        YL_13898
  # 85 アセビ_狭義                                         YL_13898
  # 86 キツネノマゴ_NA                                     YL_15775
  # 87 キツネノマゴ_NA                                     YL_15777
  # 88 ヒメアカボシタツナミ/ヤクシマナミキ_NA              YL_16392
  # 89 ヒメアカボシタツナミ/ヤクシマナミキ_NA              YL_16375
  # 90 ヤクシマシソバタツナミ/ヤクシマナミキ_NA            YL_16375
  # 91 ヤンバルツルハッカ_NA                               YL_16179
  # 92 ヤンバルツルハッカ_NA                               YL_16180

## 1対1の対応確認(各データソースのIDと学名)
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)
library(magrittr)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

jn_diff <- 
  jn %>%
  filter(scientific_name_without_author != "－") %>%
  distinct(ID, scientific_name_without_author) %>%
  separate(ID, into=c("source", "no")) %>%
  split(.$source) %>%
  map(select, -source) %>%
  map(check_one2one) %T>%
  {
    source_names <- names(.)
    source_nrow  <- map(., nrow)
    source_df <<- tibble(source=rep(source_names, source_nrow))
  } %>%
  bind_rows() %>%
  bind_cols(source_df) %>%
  filter(diff==FALSE)

jn_diff %>%
  transmute(ID=str_c(source, "_", no)) %>%
  left_join(jn) %>%
  select(! starts_with("Family"))

  #  ID       common_name                another_name               note_1  note_2 scientific_name_without_author
  #   YL_13327 チョウセンハガクレツリフネ チョウセンハガクレツリフネ 標準    外     Impatiens furcillata
  # 2 YL_13328 ヤマツリフネソウ           ヤマツリフネソウ           異分類? 外     Impatiens furcillata
  # 3 YL_13672 コチョウワビスケ           コチョウワビスケ           標準    栽     Camellia wabisuke var. campanulata f. bicolor
  # 4 YL_13673 ワビスケ                   ワビスケ                   標準    栽     Camellia wabisuke var. campanulata f. bicolor



## 学名のチェック(全データのIDと学名)
  # Hub_nameと学名とは1対1対応にはならない
  # データソースごとに採用している学名が異なるため
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)
library(magrittr)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

hub_plus_id <- 
  hub %>%
  select(! starts_with("Family")) %>%
  filter(status=="確定") %>% select(-status) %>%
  mutate(hub_plus=paste(Hub_name, lato_stricto, sep="-"), message) %>% select(-Hub_name, -lato_stricto) %>%
  pivot_longer(cols=GL:YL, names_to="source", values_to="ID", values_drop_na=TRUE) %>%
  select(hub_plus, ID) %>%
  distinct()

jn_sn_id_no_author   <- distinct(jn, sci_name=scientific_name_without_author, ID)
jn_sn_id_with_author <- distinct(jn, sci_name=scientific_name_with_author, ID)

  # 学名なし
id_sn_no_author <- 
  hub_plus_id %>%
  left_join(jn_sn_id_no_author) %>%
  distinct(hub_plus, sci_name) %>%
  filter(sci_name != "－") %>% filter(hub_plus != "－") %>% na.omit() %>%
  check_one2one() %>%
  filter(diff==FALSE)

  # 学名あり
id_sn_with_author <- 
  hub_plus_id %>%
  left_join(jn_sn_id_with_author) %>%
  distinct(hub_plus, sci_name) %>%
  filter(sci_name != "－") %>% filter(hub_plus != "－") %>% na.omit() %>%
  check_one2one() %>%
  filter(diff==FALSE)

  # 編集距離，library(MiscPsycho)を使わなくて，大丈夫そう
  # ずらすところまで
id_sn_no_author %<>%
  mutate(hub_plus2=lag(hub_plus), sci_name2=lag(sci_name)) %>%
  filter(hub_plus==hub_plus2) %>%
  select(-diff, -hub_plus2)

id_sn_with_author %<>%
  mutate(hub_plus2=lag(hub_plus), sci_name2=lag(sci_name)) %>%
  filter(hub_plus==hub_plus2) %>%
  select(-diff, -hub_plus2)

  # 編集距離(命名者なし)
dist <- 
  id_sn_no_author %$%
  map2(sci_name, sci_name2, utils::adist) %>% unlist()
id_sn_no_author %>%
  mutate(dist) %>%
  arrange(dist) %>%
  filter(dist<6) %>%
  readr::write_tsv("d:/no_author.txt")

  # 編集距離(命名者あり)
id_sn_with_author %<>%  # 命名者なしにあるものは除外
  anti_join(id_sn_no_author, by=c("hub_plus"="hub_plus"))
dist <- 
  id_sn_with_author %$%
  map2(sci_name, sci_name2, utils::adist) %>% unlist()
id_sn_with_author %>%
  mutate(dist) %>%
  arrange(dist) %>%
  filter(dist<6) %>%
  readr::write_tsv("d:/with_author.txt")


## 各Hub_name，各IDでanother_name_ID==0の無いやつ
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

no_0_id <- 
  jn %>%
  transmute(ID, another_name_ID, n=1) %>%
  distinct() %>%
  pivot_wider(names_from=another_name_ID, values_from=n, values_fill=0, names_glue="ID_{another_name_ID}") %>%
  filter(ID_0==0)

no_0_id <- 
  hub %>%
  select(all_name, GL:YL) %>%
  pivot_longer(cols=GL:YL, names_to="source", values_to="ID",  values_drop_na=TRUE) %>%
  left_join(select(no_0_id, ID), .)


## jnのanother_name_ID==0のものが，各IDで1つだけか確認
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")

jn %>%
  dplyr::group_by(ID) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is.na(another_name_ID)) %>%
  print() %>%  # another_name_IDが空欄
  dplyr::select(ID, common_name, another_name, another_name_ID) %>%
  dplyr::mutate(ID2=lag(ID)) %>%
  dplyr::filter(common_name==another_name) %>%
  dplyr::filter(ID==ID2) # 同一common_nameの2つ目でanother_name_IDが空欄


tmp_id <- 
  jn  %>%
  select(ID, common_name, another_name_ID) %>%
  filter(another_name_ID==0) %>%
  group_by(ID) %>%
  filter(n()>1) %>%
  print(n=nrow(.)) %>%
  .$ID %>%
  unique()

length(tmp_id)
jn %>%
  rownames_to_column("row_num") %>%
  filter(ID %in% tmp_id) %>%
  distinct() %>%
  print(n=nrow(.)) %>%
  select(-row_num, -another_name) %>%
  distinct() %>%
  print(n=nrow(.))


  # オオユウガギク，ツユクサシュスランが3つで，他は2つ
  # 
  # # A tibble: 44 x 12
  #    row_num ID       Family_ID Family_name      Family_name_JP common_name                   another_name                  another_name_ID  # note_1より右は省略
  #    <chr>   <chr>    <chr>     <chr>            <chr>          <chr>                         <chr>                                   <dbl>
  #  1 4656    GL_03321 166       Poaceae          イネ           ササガヤ                      ササガヤ                                    0
  #  2 4657    GL_03321 166       Poaceae          イネ           ササガヤ                      メンテンササガヤ                            0
  #  3 9641    GL_06652 387       Cornaceae        ミズキ         ヤマボウシ                    ヤマボウシ                                  0
  #  4 9642    GL_06652 387       Cornaceae        ミズキ         ヤマボウシ                    ヤエヤマヤマボウシ                          0
  #  5 12287   GL_08512 466       Asteraceae       キク           オオユウガギク                オオユウガギク                              0
  #  6 12288   GL_08512 466       Asteraceae       キク           オオユウガギク                キタガワユウガギク                          0
  #  7 12289   GL_08512 466       Asteraceae       キク           オオユウガギク                イワバノギク                                0
  #  8 14342   SF_00512 42        Thelypteridaceae ヒメシダ       メニッコウシダ×ハリガネワラビ メニッコウシダ×ハリガネワラビ             0
  #  9 14343   SF_00512 42        Thelypteridaceae ヒメシダ       メニッコウシダ×ハリガネワラビ ニッコウハリガネワラビ                     0
  # 10 15555   WF_00170 75        Aristolochiaceae ウマノスズクサ アマギカンアオイ              アマギカンアオイ                            0
  # 11 15556   WF_00170 75        Aristolochiaceae ウマノスズクサ アマギカンアオイ              シモダカンアオイ                            0
  # 12 15768   WF_00351 91        Araceae          サトイモ       ヤマグチテンナンショウ        ヤマグチテンナンショウ                      0
  # 13 15769   WF_00351 91        Araceae          サトイモ       ヤマグチテンナンショウ        イズテンナンショウ                          0
  # 14 16033   WF_00589 119       Colchicaceae     イヌサフラン   オオチゴユリ                  オオチゴユリ                                0
  # 15 16034   WF_00589 119       Colchicaceae     イヌサフラン   オオチゴユリ                  アオチゴユリ                                0
  # 16 16330   WF_00841 124       Orchidaceae      ラン           ツユクサシュスラン            ツユクサシュスラン                          0
  # 17 16331   WF_00841 124       Orchidaceae      ラン           ツユクサシュスラン            クニガミシュスラン                          0
  # 18 16332   WF_00841 124       Orchidaceae      ラン           ツユクサシュスラン            ナンカイシュスラン                          0
  # 19 16737   WF_01199 139       Arecaceae        ヤシ           ノヤシ                        ノヤシ                                      0
  # 20 16738   WF_01199 139       Arecaceae        ヤシ           ノヤシ                        セボリーヤシ                                0
  # 21 16831   WF_01273 157       Eriocaulaceae    ホシクサ       エゾホシクサ                  エゾホシクサ                                0
  # 22 16832   WF_01273 157       Eriocaulaceae    ホシクサ       エゾホシクサ                  アズミイヌノヒゲ                            0
  # 23 16840   WF_01278 157       Eriocaulaceae    ホシクサ       クシロホシクサ                クシロホシクサ                              0
  # 24 16841   WF_01278 157       Eriocaulaceae    ホシクサ       クシロホシクサ                ノソリホシクサ                              0
  # 25 17293   WF_01661 161       Cyperaceae       カヤツリグサ   キンガヤツリ                  キンガヤツリ                                0
  # 26 17294   WF_01661 161       Cyperaceae       カヤツリグサ   キンガヤツリ                  ムツオレガヤツリ                            0
  # 27 17402   WF_01755 161       Cyperaceae       カヤツリグサ   アカンテンツキ                アカンテンツキ                              0
  # 28 17403   WF_01755 161       Cyperaceae       カヤツリグサ   アカンテンツキ                オホーツクテンツキ                          0
  # 29 17717   WF_02031 166       Poaceae          イネ           タキキビ                      タキキビ                                    0
  # 30 17718   WF_02031 166       Poaceae          イネ           タキキビ                      カシマガヤ                                  0
  # 31 19885   WF_03796 206       Rosaceae         バラ           ネクタリン                    ネクタリン                                  0
  # 32 19886   WF_03796 206       Rosaceae         バラ           ネクタリン                    ズバイモモ                                  0
  # 33 19970   WF_03862 216       Fagaceae         ブナ           ニタリジイ                    ニタリジイ                                  0
  # 34 19971   WF_03862 216       Fagaceae         ブナ           ニタリジイ                    ハンスダ                                    0
  # 35 20280   WF_04075 242       Rhizophoraceae   ヒルギ         オヒルギ                      オヒルギ                                    0
  # 36 20281   WF_04075 242       Rhizophoraceae   ヒルギ         オヒルギ                      アカバナヒルギ                              0
  # 37 21428   WF_05009 333       Brassicaceae     アブラナ       コタネツケバナ                コタネツケバナ                              0
  # 38 21429   WF_05009 333       Brassicaceae     アブラナ       コタネツケバナ                コカイタネツケバナ                          0
  # 39 21614   WF_05147 346       Polygonaceae     タデ           イタドリ                      イタドリ                                    0
  # 40 21615   WF_05147 346       Polygonaceae     タデ           イタドリ                      オノエイタドリ                              0
  # 41 21617   WF_05149 346       Polygonaceae     タデ           ベニイタドリ                  ベニイタドリ                                0
  # 42 21618   WF_05149 346       Polygonaceae     タデ           ベニイタドリ                  オオメイゲツソウ                            0
  # 43 23770   WF_06895 446       Lamiaceae        シソ           ヤマハッカ                    ヤマハッカ                                  0
  # 44 23771   WF_06895 446       Lamiaceae        シソ           ヤマハッカ                    オオバヤマハッカ                            0
  # 
  # 別名(another_name)を除外して，重複をのぞいたもの
  # # A tibble: 21 x 10
  #    ID       Family_ID Family_name     Family_name_JP common_name                another_name_ID note_1 note_2 scientific_name_with_author                                                          scientific_name_without_author
  #    <chr>    <chr>     <chr>           <chr>          <chr>                                <dbl> <chr>  <chr>  <chr>                                                                                <chr>
  #  1 GL_03321 166       Poaceae         イネ           ササガヤ                                 0 <NA>   <NA>   Microstegium japonicum (Miq.) Koidz. var. somae (Hayata) T.Koyama                    Microstegium japonicum var. somae
  #  2 GL_06652 387       Cornaceae       ミズキ         ヤマボウシ                               0 <NA>   <NA>   Cornus kousa Buerger ex Hance var. chinensis Osborn                                  Cornus kousa var. chinensis
  #  3 GL_08512 466       Asteraceae      キク           オオユウガギク                           0 <NA>   <NA>   Aster sp.                                                                            Aster sp.
  #  4 SF_00512 42        Thelypteridace~ ヒメシダ       メニッコウシダ×ハリガネ~                0 <NA>   <NA>   Thelypteris japonica (Baker) Ching × T. nipponica (Franch. et Sav.) Ching var. bor~  Thelypteris japonica × T. nipponica var~
  #  5 WF_00170 75        Aristolochiace~ ウマノスズクサ アマギカンアオイ                         0 <NA>   <NA>   Asarum muramatsui Makino                                                             Asarum muramatsui
  #  6 WF_00351 91        Araceae         サトイモ       ヤマグチテンナンショウ                   0 <NA>   <NA>   Arisaema suwoense Nakai                                                              Arisaema suwoense
  #  7 WF_00589 119       Colchicaceae    イヌサフラン   オオチゴユリ                             0 <NA>   <NA>   Disporum viridescens (Maxim.) Nakai                                                  Disporum viridescens
  #  8 WF_00841 124       Orchidaceae     ラン           ツユクサシュスラン                       0 <NA>   <NA>   Goodyera foliosa (Lindl.) Benth. ex C.B.Clarke var. foliosa                          Goodyera foliosa var. foliosa
  #  9 WF_01199 139       Arecaceae       ヤシ           ノヤシ                                   0 <NA>   <NA>   Clinostigma savoryanum (Rehder et E.H.Wilson) H.E.Moore et Fosberg                   Clinostigma savoryanum
  # 10 WF_01273 157       Eriocaulaceae   ホシクサ       エゾホシクサ                             0 <NA>   <NA>   Eriocaulon miquelianum Koern. var. monococcon (Nakai) T.Koyama ex Miyam.             Eriocaulon miquelianum var. monococcon
  # 11 WF_01278 157       Eriocaulaceae   ホシクサ       クシロホシクサ                           0 <NA>   <NA>   Eriocaulon sachalinense Miyabe et Nakai var. kusiroense (Miyabe et Kudo ex Satake)~  Eriocaulon sachalinense var. kusiroense
  # 12 WF_01661 161       Cyperaceae      カヤツリグサ   キンガヤツリ                             0 <NA>   <NA>   Cyperus odoratus L.                                                                  Cyperus odoratus
  # 13 WF_01755 161       Cyperaceae      カヤツリグサ   アカンテンツキ                           0 <NA>   <NA>   Fimbristylis dichotoma (L.) Vahl var. ochotensis (Meinsh.) Honda                     Fimbristylis dichotoma var. ochotensis
  # 14 WF_02031 166       Poaceae         イネ           タキキビ                                 0 <NA>   <NA>   Phaenosperma globosum Munro ex Benth.                                                Phaenosperma globosum
  # 15 WF_03796 206       Rosaceae        バラ           ネクタリン                               0 <NA>   <NA>   －                                                                                   －
  # 16 WF_03862 216       Fagaceae        ブナ           ニタリジイ                               0 <NA>   <NA>   －                                                                                   －
  # 17 WF_04075 242       Rhizophoraceae  ヒルギ         オヒルギ                                 0 <NA>   <NA>   Bruguiera gymnorhiza (L.) Lam.                                                       Bruguiera gymnorhiza
  # 18 WF_05009 333       Brassicaceae    アブラナ       コタネツケバナ                           0 <NA>   <NA>   －                                                                                   －
  # 19 WF_05147 346       Polygonaceae    タデ           イタドリ                                 0 <NA>   <NA>   Fallopia japonica (Houtt.) Ronse Decr. var. japonica                                 Fallopia japonica var. japonica
  # 20 WF_05149 346       Polygonaceae    タデ           ベニイタドリ                             0 <NA>   <NA>   －                                                                                   －
  # 21 WF_06895 446       Lamiaceae       シソ           ヤマハッカ                               0 <NA>   <NA>   Isodon inflexus (Thunb.) Kudo                                                        Isodon inflexus



## 科名がHubとjnで同一かどうかの確認
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

hub <- 
  hub %>%
  pivot_longer(cols=GL:YL, names_to="source", values_to="ID", values_drop_na=TRUE) %>%
  select(ID, Family_ID, Family_name, Family_name_JP) %>%
  distinct()

jn <- 
  jn %>%
  select(ID, Family_ID, Family_name, Family_name_JP) %>%
  distinct()

anti_a <- anti_join(hub, jn) %>%arrange(ID)        %>% print(n=nrow(.)) 
anti_b <- anti_join(jn, hub) %>%arrange(Family_ID) %>% print(n=nrow(.)) 

setdiff(anti_a$ID, anti_b$ID)
setdiff(anti_b$ID, anti_a$ID) %>%
  tibble(ID=.) %>%
  left_join(anti_b)

## 科名がHubとjnで同一かどうかの確認
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

tibble(ID=c("GL_07583", "WF_06318", "YL_15092", "YL_18729", "GL_09336", "GL_00643", "SF_00457", 
  "YL_00926", "YL_12981", "WF_05375", "YL_01645", "WF_02646", "WF_06627", "YL_15647", 
  "GL_05627", "YL_10599", "WF_04384", "WF_02298", "YL_06348", "GL_03463", "GL_00727", 
  "SF_01028", "YL_01049", "WF_05704", "GL_06780", "YL_13574", "GL_02129", "YL_03487")) %>%
  left_join(jn) %>%
  select(! starts_with("Family")) %>%
  distinct() %>%
  print(n=nrow(.))

## 1つ目にあるのに，common_name == another_name じゃないやつ
library(tidyverse)
colnames_replace_all <- function(df, pattern, replacement) {
  cnames <- colnames(df)
  colnames(df) <- stringr::str_replace_all(cnames, pattern, replacement)
  df
}

cl_file <- "wamei_checklist_ver.1.10.xlsx" # check list file      和名チェックリストのファイル
cl_dir  <-  "D:/matu/work/tmp/veg/"         # check list directory 上記ファイルのディレクトリ
path <- stringr::str_c(cl_dir, cl_file)
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all(" ", "_")

  # sort(intersect(cn, an))までのコメント部分は送付せず(松村の作業用)
  # 
  # cn <- 
  #   jn %>%
  #     dplyr::filter(common_name!=another_name) %>%
  #     dplyr::filter(str_detect(ID, "GL")) %>%
  #   .$common_name
  # an <- 
  #   jn %>%
  #     dplyr::filter(common_name!=another_name) %>%
  #     dplyr::filter(str_detect(ID, "GL")) %>%
  #   .$another_name %>%
  #   unique()
  # sort(intersect(cn, an))
  # 
  # cn <- 
  #   jn %>%
  #     dplyr::filter(common_name!=another_name) %>%
  #     dplyr::filter(str_detect(ID, "YL")) %>%
  #   .$common_name
  # an <- 
  #   jn %>%
  #     dplyr::filter(common_name!=another_name) %>%
  #     dplyr::filter(str_detect(ID, "YL")) %>%
  #   .$another_name %>%
  #   unique()
  # sort(intersect(cn, an))



  # another_name_IDが空欄のもののチェック
  #   1つ目にあるのに，common_name != another_name のやつがあるけど，問題なし?
  #   common_name==another_nameの入力漏れ?
tmp <- 
  jn %>%
  dplyr::group_by(ID) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is.na(another_name_ID)) %>%
  dplyr::mutate(ID2=lag(ID)) %>%
  dplyr::filter(common_name!=another_name) %>%
  dplyr::filter(ID!=ID2) %>%
  .$common_name
jn %>%
  tibble::rownames_to_column("row_num") %>%
  dplyr::filter(common_name %in% tmp) %>%
  dplyr::select(row_num, ID, common_name, another_name, another_name_ID, note_1, note_2, scientific_name_with_author)

  #  ** の1行前に common_name == another_nameが必要?，他は問題なし
  # 
  #         row_num ID       common_name      another_name           another_name_ID note_1 note_2 scientific_name_with_author
  #         <chr>   <chr>    <chr>            <chr>                  <chr>           <chr>  <chr>  <chr>
  #  **   1 31368   YL_04223 ゾウゲヤシ       オオミゾウゲヤシ       <NA>            標準   外     Phytelephas macrocarpa Ruiz et Pav.
  #       2 31369   YL_04223 ゾウゲヤシ       コミノゾウゲヤシ       <NA>            標準   外     Phytelephas macrocarpa Ruiz et Pav.
  #  **   3 37736   YL_08621 トウツルキンバイ シロツルキンバイ       <NA>            標準   外     Argentina anserina (L.) Rydb. var. anserna
  #       4 37737   YL_08621 トウツルキンバイ ヨウシュノツルキンバイ <NA>            標準   外     Argentina anserina (L.) Rydb. var. anserna
  #  **   5 51855   YL_18194 タニヒゴタイ     アツバモリヒゴタイ     <NA>            標準   外     Saussurea sinuata Kom.
  #       6 51856   YL_18194 タニヒゴタイ     キクバモリアザミ       <NA>            標準   外     Saussurea sinuata Kom.
  #       7 51857   YL_18194 タニヒゴタイ     キクバモリヒゴタイ     <NA>            標準   外     Saussurea sinuata Kom.
  #       8 51858   YL_18194 タニヒゴタイ     ハハキヒゴタイ         <NA>            標準   外     Saussurea sinuata Kom.


  # another_name_IDはあるやつのチェック
  #   1つ目にあるのに，common_name != another_name のやつがあるけど，問題なし?
  #   common_name==another_nameの入力漏れ?
tmp <- 
  jn %>%
  dplyr::filter( !is.na(another_name_ID) ) %>%
  dplyr::mutate(ID2=lag(ID)) %>%
  dplyr::filter(common_name!=another_name) %>%
  dplyr::filter(ID!=ID2) %>%
  .$common_name
jn %>%
  tibble::rownames_to_column("row_num") %>%
  dplyr::filter(common_name %in% tmp) %>%
  dplyr::select(row_num, ID, common_name, another_name, another_name_ID, note_1, note_2, scientific_name_with_author)

  #  ** の1行前に common_name == another_nameが必要?，他は問題なし
  #
  #         row_num ID       common_name            another_name           another_name_ID note_1 note_2 scientific_name_with_author
  #         <chr>   <chr>    <chr>                  <chr>                  <chr>           <chr>  <chr>  <chr>
  #       1 144     GL_00099 オオバヤシャゼンマイ   オオバヤシャゼンマイ   0               <NA>   <NA>   Osmunda × intermedia (Honda) Sugim.
  #       2 145     GL_00099 オオバヤシャゼンマイ   オクタマゼンマイ       1               <NA>   <NA>   Osmunda × intermedia (Honda) Sugim.
  #       3 357     GL_00246 ニシノコハチジョウシダ ニシノコハチジョウシダ 0               <NA>   <NA>   Pteris kiuschiuensis Hieron.
  #       4 358     GL_00246 ニシノコハチジョウシダ コハチジョウシダ       1               <NA>   <NA>   Pteris kiuschiuensis Hieron.
  #  **   5 13866   SF_00131 オオバヤシャゼンマイ   オクタマゼンマイ       1               <NA>   <NA>   Osmunda × intermedia (Honda) Sugim.
  #  **   6 14113   SF_00323 ニシノコハチジョウシダ コハチジョウシダ       1               <NA>   <NA>   Pteris kiuschiuensis Hieron.
  #       7 25834   YL_00148 オオバヤシャゼンマイ   オオバヤシャゼンマイ   0               標準   <NA>   Osmunda × intermedia (Honda) Sugim.
  #       8 25835   YL_00148 オオバヤシャゼンマイ   オクタマゼンマイ       1               標準   <NA>   Osmunda × intermedia (Honda) Sugim.
  #       9 26165   YL_00399 ニシノコハチジョウシダ ニシノコハチジョウシダ 0               標準   <NA>   Pteris kiuschiuensis Hieron.
  #      10 26166   YL_00399 ニシノコハチジョウシダ コハチジョウシダ       1               標準   <NA>   Pteris kiuschiuensis Hieron.

## Hub_nameとidの組み合わせがおかしい(Hub_dataシートの確認)
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

  # 同一Hub_name + 同一ID：statusに未統合と確定の両方があるがOKか?
a <- hub %>% transmute(hub_plus=paste(Hub_name, lato_stricto, sep="-"), GL, SF, WF, YL, status) %>% distinct() %>% .$hub_plus %>% table()
b <- hub %>% transmute(hub_plus=paste(Hub_name, lato_stricto, sep="-"), GL, SF, WF, YL)         %>% distinct() %>% .$hub_plus %>% table()
a[a != b]  # 
b[a != b]  # 

  # 同一Hub_name(広義・狭義含む)だが，IDの組み合わせ表記が異なる
hub_id_diff <- 
  hub %>% 
  transmute(hub_plus=paste(Hub_name, lato_stricto, sep="-"), GL, SF, WF, YL, status) %>%
  distinct() %>%
  group_by(hub_plus) %>%
  filter(n()>1) %>%
  pivot_longer(cols=GL:YL, names_to="source", values_to="ID",  values_drop_na=TRUE) %>%
  group_by(hub_plus, ID) %>%
  filter(n()>1) %>%
  print(n=nrow(.)) %>%  # 一旦出力
  ungroup() %>%
  select(hub_plus) %>%
  distinct() %>%
  separate(hub_plus, into=c("Hub_name", NA), sep="-") %>%
  left_join(hub) %>%
  print(n=nrow(.))


hub_name + lato/stricto       idの組み合わせ
 # 学名は全て同じ(単純な入力ミス?)
オオバハマアサガオ_NA          GL_07583..............WF_06318...........
オオバハマアサガオ_NA          GL_07583..............WF_06318...YL_15092

 # 学名が異なるので，組み合わせがおかしい?
オキナワトベラ_NA              .................................YL_18729
オキナワトベラ_NA              GL_09336.........................YL_18729

 # YL_00926は学名が異なるので，組み合わせがおかしい?
ゲジゲジシダ_NA                GL_00643...SF_00457......................
ゲジゲジシダ_NA                GL_00643...SF_00457..............YL_00926

 # 学名は同じで，JN_datasetのシートのcommon_nameがテンサイとサトウダイコンの2つある
 # JN_datasetのシートの間違い?
サトウダイコン/テンサイ_狭義   .................................YL_12981
サトウダイコン/テンサイ_狭義   ......................WF_05375...........

 # 異なるものに同じHub_name(マツ科とキンポウゲ科)
シベリアカラマツ_NA            .................................YL_01645
シベリアカラマツ_NA            ......................WF_02646...........

 # 学名が異なるので，組み合わせがおかしい?
シベリアクガイソウ_NA          ......................WF_06627...........
シベリアクガイソウ_NA          ......................WF_06627...YL_15647

 # WF_04384は学名が異なるので，組み合わせがおかしい?
タカオスミレ_NA                GL_05627.........................YL_10599
タカオスミレ_NA                GL_05627..............WF_04384...YL_10599

 # GL_03463は学名が異なるので，組み合わせがおかしい?
ヌメリグサ_NA                  ......................WF_02298...YL_06348
ヌメリグサ_NA                  GL_03463..............WF_02298...YL_06348

 # YL_01049は学名が異なるので，組み合わせがおかしい?
ホソコバカナワラビ_NA          GL_00727...SF_01028......................
ホソコバカナワラビ_NA          GL_00727...SF_01028..............YL_01049

 # 3つとも別の学名なので，組み合わせがおかしい?
ユキワリコザクラ_NA            ......................WF_05704...........
ユキワリコザクラ_NA            GL_06780..............WF_05704...YL_13574

 # 学名が異なるので，組み合わせがおかしい?
リトウトンボ_NA                GL_02129.................................
リトウトンボ_NA                GL_02129.........................YL_03487

 # 学名が異なるが同じHub_name
ベチベル/ベチベルソウ_NA       ......................WF_02187...........
ベチベル/ベチベルソウ_NA       .................................YL_05693


  # 以下は全て「未統合」のものなので，OK
  # アオツヅラフジ/ホウザンツヅラフジ_NA  ......................WF_02379...........
  # アオツヅラフジ/ホウザンツヅラフジ_NA  GL_03685.........................YL_06725
  # アオツヅラフジ/ホウザンツヅラフジ_NA  GL_03686.........................YL_06726
  # アキザキナギラン/オオナギラン_NA  ......................WF_00760...YL_03092
  # アキザキナギラン/オオナギラン_NA  GL_01893..............WF_00759...YL_03083
  # イトザクラ_NA  .................................YL_08654
  # イトザクラ_NA  ......................WF_03709...YL_08655
  # オオハシバミ/ハシバミ_NA  GL_05253..............WF_03973...YL_09973
  # オオハシバミ/ハシバミ_NA  GL_05254.........................YL_09974
  # オオフジシダ_NA  ...........SF_00272..............YL_00477
  # オオフジシダ_NA  GL_00298...SF_00270..............YL_00475
  # オランダナデシコ/カーネーション_NA  .................................YL_12733
  # オランダナデシコ/カーネーション_NA  ......................WF_05272...YL_12705
  # キツネノマゴ_NA  .................................YL_15775
  # キツネノマゴ_NA  GL_07818..............WF_07034...YL_15777
  # コマツナギ/トウコマツナギ_NA  ......................WF_03086...YL_08131
  # コマツナギ/トウコマツナギ_NA  GL_04384.........................YL_08144
  # ジンボソウ/セイタカヌカボシソウ/ナスヌカボシソウ_NA  ......................WF_01318...YL_04567
  # ジンボソウ/セイタカヌカボシソウ/ナスヌカボシソウ_NA  GL_02498.................................
  # ジンボソウ/セイタカヌカボシソウ/ナスヌカボシソウ_NA  GL_02499.................................
  # センダン/タイワンセンダン_NA  .................................YL_11781
  # センダン/タイワンセンダン_NA  .................................YL_11782
  # センダン/タイワンセンダン_NA  ......................WF_04846...YL_11777
  # センダン/タイワンセンダン_NA  GL_06103.................................
  # タイワンスベリヒユ_NA  ......................WF_05470...........
  # タイワンスベリヒユ_NA  GL_06551..............WF_05471...YL_13127
  # ハチジョウイチゴ×カジイチゴ/ヒメシマカジイチゴ_NA  GL_04847.........................YL_09241
  # ハチジョウイチゴ×カジイチゴ/ヒメシマカジイチゴ_NA  GL_04850.................................
  # ハライヌノヒゲ/ユキイヌノヒゲ_NA  GL_02428.................................
  # ハライヌノヒゲ/ユキイヌノヒゲ_NA  GL_02446..............WF_01272...YL_04493
  # ハライヌノヒゲ/ユキイヌノヒゲ_NA  GL_02460.................................
  # ヒメアカボシタツナミ/ヤクシマナミキ_NA  GL_08087..............WF_06746...YL_16375
  # ヒメアカボシタツナミ/ヤクシマナミキ_NA  GL_08099.........................YL_16392
  # ビランジ_NA  .................................YL_12846
  # ビランジ_NA  GL_06475..............WF_05334...YL_12848
  # ホクセンイチゴツナギ/ホクセンナガハグサ_NA  .................................YL_06307
  # ホクセンイチゴツナギ/ホクセンナガハグサ_NA  GL_03438.........................YL_06284
  # ヤンバルツルハッカ_NA  .................................YL_16179
  # ヤンバルツルハッカ_NA  ......................WF_06798...YL_16180
  # アマクサシダ_NA  .................................YL_00390
  # アマクサシダ_NA  GL_00256...SF_00308......................
  # アリサンクスクスラン/クスクスヨウラクラン_NA  .................................YL_03383
  # アリサンクスクスラン/クスクスヨウラクラン_NA  ......................WF_00927...........
  # アリサンクスクスラン/クスクスヨウラクラン_NA  GL_02087.................................
  # イワヘゴ_NA  .................................YL_01115
  # イワヘゴ_NA  GL_00783...SF_00873......................
  # エゾメシダ_NA  .................................YL_00690
  # エゾメシダ_NA  GL_00410...SF_00559......................
  # オオアマクサシダ_NA  .................................YL_00413
  # オオアマクサシダ_NA  GL_00236...SF_00309......................
  # オオイタチシダ_NA  .................................YL_01143
  # オオイタチシダ_NA  GL_00808...SF_00900......................
  # オオバタネツケバナ_NA  .................................YL_12180
  # オオバタネツケバナ_NA  GL_06209..............WF_05011...........
  # オオブドウホオズキ_NA  .................................YL_15164
  # オオブドウホオズキ_NA  ......................WF_06358...........
  # カキバカンコノキ_NA  ......................WF_04172...........
  # カキバカンコノキ_NA  GL_05835.................................
  # カタバミ_NA  ......................WF_04053...........
  # カタバミ_NA  GL_05351.................................
  # カマツカ_NA  ......................WF_03787...........
  # カマツカ_NA  GL_04658.........................YL_08973
  # キンミズヒキ_NA  ......................WF_03504...........
  # キンミズヒキ_NA  GL_04547.........................YL_08608
  # クガイソウ_NA  ......................WF_06625...........
  # クガイソウ_NA  GL_07772.........................YL_15642
  # クルマバナ_NA  ......................WF_06824...YL_16041
  # クルマバナ_NA  GL_07928.................................
  # クロヅル/タイワンクロヅル_NA  .................................YL_10172
  # クロヅル/タイワンクロヅル_NA  ......................WF_04048...........
  # ケナガバヤブマオ_NA  .................................YL_09619
  # ケナガバヤブマオ_NA  GL_05063..............WF_03446...........
  # コウリョウカモジグサ/タイシャクカモジ_NA  .................................YL_05827
  # コウリョウカモジグサ/タイシャクカモジ_NA  GL_03225..............WF_01980...........
  # コシダ_NA  .................................YL_00212
  # コシダ_NA  GL_00146...SF_00178......................
  # ササガヤ_NA  ......................WF_02239...YL_06009
  # ササガヤ_NA  GL_03321.................................
  # シベリアホザキノフサモ/トゲホザキノフサモ_NA  ......................WF_02931...........
  # シベリアホザキノフサモ/トゲホザキノフサモ_NA  GL_04268.........................YL_07764
  # タイワンシノブ_NA  .................................YL_01419
  # タイワンシノブ_NA  ...........SF_01224......................
  # タカネアオヤギソウ_NA  ......................WF_00579...........
  # タカネアオヤギソウ_NA  GL_01676.........................YL_02750
  # タカネトウウチソウ_NA  .................................YL_09269
  # タカネトウウチソウ_NA  ......................WF_03675...........
  # タカネトウウチソウ_NA  GL_04876.................................
  # タネツケバナ_NA  ......................WF_05007...YL_12174
  # タネツケバナ_NA  GL_06198.................................
  # ナガバキタアザミ_NA  .................................YL_18177
  # ナガバキタアザミ_NA  ......................WF_07390...........
  # ナガバキタアザミ_NA  GL_09054.................................
  # ナメラサギソウ/リュウキュウサギソウ_NA  ......................WF_00848...........
  # ナメラサギソウ/リュウキュウサギソウ_NA  GL_02000.........................YL_03266
  # ヌカボシクリハラン_NA  .................................YL_01512
  # ヌカボシクリハラン_NA  GL_01031...SF_01305......................
  # ハイヌメリグサ_NA  ......................WF_02297...YL_06349
  # ハイヌメリグサ_NA  GL_03462.................................
  # ハエドクソウ_NA  .................................YL_16489
  # ハエドクソウ_NA  ......................WF_06924...........
  # ハエドクソウ_NA  GL_08145.................................
  # ハマヤブマオ_NA  ......................WF_03452...YL_09612
  # ハマヤブマオ_NA  GL_05064.................................
  # バイケイソウ_NA  .................................YL_02761
  # バイケイソウ_NA  GL_01672..............WF_00575...........
  # ヒメミズトンボ_NA  ......................WF_00855...........
  # ヒメミズトンボ_NA  GL_02004.................................
  # ヒロハノハネガヤ_NA  .................................YL_06152
  # ヒロハノハネガヤ_NA  GL_03609.................................
  # ベチベル/ベチベルソウ_NA  .................................YL_05693
  # ベチベル/ベチベルソウ_NA  ......................WF_02187...........
  # ヤクカナワラビ_NA  .................................YL_01030
  # ヤクカナワラビ_NA  GL_00698...SF_00986......................
  # ヤハズカワツルモ_NA  ......................WF_00484...YL_02595
  # ヤハズカワツルモ_NA  GL_01570.................................
  # ヤマクルマバナ_NA  .................................YL_16039
  # ヤマクルマバナ_NA  ......................WF_06828...........
  # ヤマクルマバナ_NA  GL_07931.................................
  # ヤマボウシ_NA  ......................WF_05484...YL_13316
  # ヤマボウシ_NA  GL_06652.................................

## Hub_nameと各リストのIDが1対1対応か確認(広義/狭義情報も含む)
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # 以下のものは山ノ内さんには送っていない
  #     やってみたが，やる意味があるのか不明
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

check_hub_ID <- function(hub, wl){
  wlx <- str_c(wl, ".x")
  wly <- str_c(wl, ".y")
  hub %>% 
    filter(!is.na(.data[[wl]])) %>% 
    filter(status != "！未統合") %>% 
    mutate(Hub_name=str_c(Hub_name, replace_na(lato_stricto, ""))) %>%
    select(Hub_name, all_of(wl)) %>% 
    distinct() %>% 
    left_join(., .,by=c("Hub_name"="Hub_name")) %>% 
    filter(.data[[wlx]] != .data[[wly]]) %>% 
    arrange(Hub_name)
}

  # 1対1でないものがあるが，
  #    全て「！未統合」のものだけなので，OK
check_hub_ID(hub, wl="GL")
check_hub_ID(hub, wl="SF")
check_hub_ID(hub, wl="WF")
check_hub_ID(hub, wl="YL")


  # hub_nameに対して，2つ以上のIDのもの
  #   all_nameが2以上のものばっかりなので，OK
  # 
  # 未統合，広義/狭義を考えない場合
check_hub_ID <- function(hub, wl){
  wlx <- str_c(wl, ".x")
  wly <- str_c(wl, ".y")
  hub %>% 
    filter(!is.na(.data[[wl]])) %>% 
    select(Hub_name, all_of(wl)) %>% 
    distinct() %>% 
    left_join(., .,by=c("Hub_name"="Hub_name")) %>% 
    filter(.data[[wlx]] != .data[[wly]]) %>% 
    arrange(Hub_name)
}
  # 1対1でないものがあるが，
  #    全て「！未統合」のものだけなので，たぶんOK
gl_dup <- check_hub_ID(hub, wl="GL") %>% .$Hub_name
sf_dup <- check_hub_ID(hub, wl="SF") %>% .$Hub_name
wf_dup <- check_hub_ID(hub, wl="WF") %>% .$Hub_name
yl_dup <- check_hub_ID(hub, wl="YL") %>% .$Hub_name

unique(c(gl_dup, sf_dup, wf_dup, yl_dup)) %>%
  tibble(Hub_name=.) %>%
  dplyr::left_join(hub) %>%
  group_by(Hub_name) %>%
  filter(n()==1)

## another_nameとall_nameの比較(既に実施していた)
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)

source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "")
jn <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>% colnames_replace_all("[()]", "") %>%
    fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

setdiff(jn$another_name, hub$all_name)
setdiff(hub$all_name, jn$another_name)


## hub_nameに対する各IDデータソースのID(別でチェック済み)
rm(list=ls(all=TRUE)); gc(); gc()
library(tidyverse)
library(readxl)
source("D:/matu/work/ToDo/wameicheckr/R/wamei_check_fun.r")

    # allデータ
path <- "D:/matu/work/ToDo/wameicheckr/data/wamei_checklist_ver.1.10.xlsx"
hub_master <-readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>% colnames_replace_all("[ /]", "_") %>% 
  colnames_replace_all("[()]", "")
jn_master <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% colnames_replace_all("[ /]", "_") %>%
   colnames_replace_all("[()]", "") %>% fill_another_name_id()  # another_name_id が 空欄のものを自動で入れる

full <- wamei_master2full(hub_master, jn_master)
hub  <- wamei_hub(full)
id   <- wamei_id(full)

  # すべて未統合
  #   -> 未統合のものは，とりあえず別のhub_nameの方が良い
id_dup <- check_dup_id(id)
id_dup %>%
  select(hub_plus) %>%
  left_join(hub) %>%
  print(n=nrow(.))



``` r
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.0.6     v dplyr   1.0.4
#> v tidyr   1.1.2     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(wameicheckr)
library(magrittr)
#> 
#> Attaching package: 'magrittr'
#> The following object is masked from 'package:purrr':
#> 
#>     set_names
#> The following object is masked from 'package:tidyr':
#> 
#>     extract
```

維管束植物和名チェックリストはパッケージ内にある．
読み込んだあとで，使いやすくするために，若干の整理を実行

``` r
data(hub_master)
data(jn_master)
hub_master <- 
  hub_master %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~str_replace_all(., "[()]", "")) %>%
  print()
#> # A tibble: 30,430 x 12
#>    all_name   Hub_name   lato_stricto Family_ID Family_name Family_name_JP GL   
#>    <chr>      <chr>      <chr>        <chr>     <chr>       <chr>          <chr>
#>  1 アスヒカズラ~ アスヒカズラ~ <NA>         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  2 イヌヤチスギラン~ イヌヤチスギラン~ <NA>         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  3 イワヒモ   ヒモラン   <NA>         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  4 ウチワマンネンスギ~ マンネンスギ~ <NA>         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  5 エゾコスギラン~ エゾコスギラン~ <NA>         1         Lycopodiac~ ヒカゲノカズラ <NA> 
#>  6 エゾノコスギラン~ コスギラン 広義         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  7 エゾヒカゲノカズラ~ ヒカゲノカズラ~ 広義         1         Lycopodiac~ ヒカゲノカズラ <NA> 
#>  8 エゾヒカゲノカズラ~ エゾヒカゲノカズラ~ <NA>         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  9 オオスギカズラ~ オオスギカズラ~ <NA>         1         Lycopodiac~ ヒカゲノカズラ <NA> 
#> 10 オニトウゲシバ~ オニトウゲシバ~ <NA>         1         Lycopodiac~ ヒカゲノカズラ <NA> 
#> # ... with 30,420 more rows, and 5 more variables: SF <chr>, WF <chr>,
#> #   YL <chr>, status <chr>, message <chr>

jn_master <- 
  jn_master %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~str_replace_all(., "[()]", "")) %>%
  fill_another_name_id() %>% # another_name_id の空欄を埋める
  print()
#> # A tibble: 53,222 x 11
#>    ID       Family_ID Family_name   Family_name_JP common_name    another_name  
#>    <chr>    <chr>     <chr>         <chr>          <chr>          <chr>         
#>  1 GL_00001 1         Lycopodiaceae ヒカゲノカズラ ヒメスギラン   ヒメスギラン  
#>  2 GL_00002 1         Lycopodiaceae ヒカゲノカズラ コスギラン     コスギラン    
#>  3 GL_00002 1         Lycopodiaceae ヒカゲノカズラ コスギラン     エゾノコスギラン~
#>  4 GL_00002 1         Lycopodiaceae ヒカゲノカズラ コスギラン     チシマスギラン
#>  5 GL_00003 1         Lycopodiaceae ヒカゲノカズラ トウゲシバ     トウゲシバ    
#>  6 GL_00003 1         Lycopodiaceae ヒカゲノカズラ トウゲシバ     ホソバトウゲシバ~
#>  7 GL_00003 1         Lycopodiaceae ヒカゲノカズラ トウゲシバ     ヒロハトウゲシバ~
#>  8 GL_00003 1         Lycopodiaceae ヒカゲノカズラ トウゲシバ     オニトウゲシバ
#>  9 GL_00004 1         Lycopodiaceae ヒカゲノカズラ コスギトウゲシバ~ コスギトウゲシバ~
#> 10 GL_00005 1         Lycopodiaceae ヒカゲノカズラ イヌヤチスギラン~ イヌヤチスギラン~
#> # ... with 53,212 more rows, and 5 more variables: another_name_ID <dbl>,
#> #   note_1 <chr>, note_2 <chr>, scientific_name_with_author <chr>,
#> #   scientific_name_without_author <chr>
```

最新版が異なる場合は，ウェブページからダウンロードして使用することも可能．

``` r
  # Download wamei chek list form https://www.gbif.jp/v2/activities/wamei_checklist.html
  # Change file name (version)
  # Set your directory by setwd()
library(readxl)

path <- "wamei_checklist_ver.1.10.xlsx"

hub_master <-
  readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>%
  dplyr::rename_with(~str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~str_replace_all(., "[()]", ""))

jn_master <- 
  readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>% 
  dplyr::rename_with(~str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~str_replace_all(., "[()]", "")) %>%
  fill_another_name_id()  # another_name_id の空欄を埋める
```

和名チェックリスト ver.1.10のエラーの修正．
バージョンアップで，修正される予定．

``` r
  # (和名チェックリスト ver.1.10への対応) another_name_IDに0がないもの
no_id_0 <- 
  c("SF_00131", "SF_00323", "WF_01542", "WF_02219", "WF_04287", 
  "SF_00127","WF_01902","WF_03825","YL_11456","YL_17759")
jn_master$another_name_ID[jn_master$ID %in% no_id_0 & jn_master$another_name_ID != 0] <- 0

  # (和名チェックリスト ver.1.10への対応) シベリアカラマツ(別科同名)
hub_master$Hub_name[
  hub_master$Hub_name=="シベリアカラマツ" &
  hub_master$Family_name_JP=="マツ"] <- 
  "シベリアカラマツ(マツ科)"
hub_master$Hub_name[
  hub_master$Hub_name=="シベリアカラマツ" &
  hub_master$Family_name_JP=="キンポウゲ"] <- 
  "シベリアカラマツ(キンポウゲ科)"
jn_master$Family_name_JP[
  jn_master$Family_name_JP=="ツルボラン"] <- 
  "ワスレグサ"
```

x1は，和名チェックリストにある和名をすべて抽出．
多いので，下記では最初の50だけ． x2は，色々なパターンを含む和名．

``` r
  # hubやjnの種名を抽出(全種)
x1 <- 
  c(hub_master$all_name, hub_master$Hub_name, jn_master$common_name, jn_master$another_name) %>%
  purrr::map(str_split, "/") %>%
  unlist() %>% unique() %>% sort() %>%
  c("だみーの和名", .)

  # 和名の例
x2 <- c("だみー", "ススキ", "ハリガネワラビ", "オミナエシ", "コナスビ", "カナビキソウ", 
  "ヤイトバナ", "チガヤ", "キジムシロ", "ハエドクソウ", "キツネノマゴ", "シロヨメナ", 
  "オオフジシダ", "コマツナギ", "アイヌタチツボスミレ", "シベリアカラマツ", "アオイモドキ")
```

入力和名に対する和名・学名の候補を出力する．
なお，x1は多いので，最初の50だけを使用．

``` r
wamei_check(x1[1:50], hub_master, jn_master)
#> # A tibble: 50 x 23
#>    input  n_match hub_plus    status  Family_ID Family_name Family_name_JP WF_ID
#>    <chr>  <chr>   <chr>       <chr>   <chr>     <chr>       <chr>          <chr>
#>  1 だみーの和~ 0       該当なし    該当なし~ -         -           -              -    
#>  2 mulbe~ 1       マグワ      確定    213       Moraceae    クワ           WF_0~
#>  3 no na~ 0       該当なし    該当なし~ -         -           -              -    
#>  4 no_na~ 1       no_named_G~ 確定    101       Potamogeto~ ヒルムシロ     -    
#>  5 no_na~ 1       ムカゴサイシンモドキ~ 確定    124       Orchidaceae ラン           WF_0~
#>  6 no_na~ 1       ミチノクサナギイチゴ~ 確定    206       Rosaceae    バラ           WF_0~
#>  7 no_na~ 1       ヤエガワカンバ狭義/~ 確定    221       Betulaceae  カバノキ       -    
#>  8 no_na~ 1       no_named_G~ 確定    221       Betulaceae  カバノキ       -    
#>  9 no_na~ 1       no_named_G~ 確定    416       Gentianace~ リンドウ       -    
#> 10 no_na~ 1       no_named_G~ 確定    466       Asteraceae  キク           -    
#> # ... with 40 more rows, and 15 more variables: YL_ID <chr>, GL_ID <chr>,
#> #   SF_ID <chr>, WF_common_name <chr>, YL_common_name <chr>,
#> #   GL_common_name <chr>, SF_common_name <chr>,
#> #   WF_scientific_name_with_author <chr>, YL_scientific_name_with_author <chr>,
#> #   GL_scientific_name_with_author <chr>, SF_scientific_name_with_author <chr>,
#> #   WF_scientific_name_without_author <chr>,
#> #   YL_scientific_name_without_author <chr>,
#> #   GL_scientific_name_without_author <chr>,
#> #   SF_scientific_name_without_author <chr>
wamei_check(x1[1:50], hub_master, jn_master, wide=FALSE)
#> # A tibble: 111 x 12
#>    input    n_match hub_plus        status  source ID     Family_ID Family_name 
#>    <chr>    <chr>   <chr>           <chr>   <chr>  <chr>  <chr>     <chr>       
#>  1 だみーの和名~ 0       該当なし        該当なし~ -      -      -         -           
#>  2 mulberry 1       マグワ          確定    WF     WF_03~ 213       Moraceae    
#>  3 mulberry 1       マグワ          確定    YL     YL_09~ 213       Moraceae    
#>  4 no named 0       該当なし        該当なし~ -      -      -         -           
#>  5 no_name~ 1       no_named_GL01   確定    GL     GL_01~ 101       Potamogeton~
#>  6 no_name~ 1       ムカゴサイシンモドキ/no ~ 確定    GL     GL_02~ 124       Orchidaceae 
#>  7 no_name~ 1       ムカゴサイシンモドキ/no ~ 確定    WF     WF_00~ 124       Orchidaceae 
#>  8 no_name~ 1       ムカゴサイシンモドキ/no ~ 確定    YL     YL_03~ 124       Orchidaceae 
#>  9 no_name~ 1       ミチノクサナギイチゴ/no ~ 確定    GL     GL_04~ 206       Rosaceae    
#> 10 no_name~ 1       ミチノクサナギイチゴ/no ~ 確定    WF     WF_03~ 206       Rosaceae    
#> # ... with 101 more rows, and 4 more variables: Family_name_JP <chr>,
#> #   common_name <chr>, scientific_name_with_author <chr>,
#> #   scientific_name_without_author <chr>
wamei_check(x2,       hub_master, jn_master,             ds=c(GL, SF, WF))
#> # A tibble: 17 x 19
#>    input   n_match hub_plus    status Family_ID Family_name Family_name_JP WF_ID
#>    <chr>   <chr>   <chr>       <chr>  <chr>     <chr>       <chr>          <chr>
#>  1 だみー  0       該当なし    該当なし~ -         -           -              -    
#>  2 ススキ  1       ススキ      確定   166       Poaceae     イネ           WF_0~
#>  3 ハリガネワラ~ 1       ハリガネワラビ~ 確定   42        Thelypteri~ ヒメシダ       -    
#>  4 オミナエシ~ 1       オミナエシ  確定   472       Caprifolia~ スイカズラ     WF_0~
#>  5 コナスビ~ 1       コナスビ狭義~ 確定   398       Primulaceae サクラソウ     WF_0~
#>  6 カナビキソウ~ 1       カナビキソウ~ 確定   339       Santalaceae ビャクダン     WF_0~
#>  7 ヤイトバナ~ 1       ヘクソカズラ~ 確定   415       Rubiaceae   アカネ         WF_0~
#>  8 チガヤ  1       チガヤ狭義  確定   166       Poaceae     イネ           WF_0~
#>  9 キジムシロ~ 2       キジムシロ狭義/広義~ 確定   206       Rosaceae    バラ           WF_0~
#> 10 ハエドクソウ~ 2       ハエドクソウ~ ！未統合~ 448       Phrymaceae  ハエドクソウ   WF_0~
#> 11 キツネノマゴ~ 1       キツネノマゴ~ ！未統合~ 440       Acanthaceae キツネノマゴ   WF_0~
#> 12 シロヨメナ~ 2       シロヨメナ/チョウセ~ ！未統合~ 466       Asteraceae  キク           WF_0~
#> 13 オオフジシダ~ 2       オオフジシダ~ ！未統合~ 31        Dennstaedt~ コバノイシカグマ~ -    
#> 14 コマツナギ~ 2       コマツナギ/トウコマ~ ！未統合~ 203       Fabaceae    マメ           WF_0~
#> 15 アイヌタチツ~ 2       アイヌタチツボスミレ~ 確定   263       Violaceae   スミレ         WF_0~
#> 16 シベリアカラ~ 1       シベリアカラマツ(キ~ 確定   174       Ranunculac~ キンポウゲ     WF_0~
#> 17 アオイモドキ~ 1       エノキアオイ~ 確定   310       Malvaceae   アオイ         WF_0~
#> # ... with 11 more variables: GL_ID <chr>, SF_ID <chr>, WF_common_name <chr>,
#> #   GL_common_name <chr>, SF_common_name <chr>,
#> #   WF_scientific_name_with_author <chr>, GL_scientific_name_with_author <chr>,
#> #   SF_scientific_name_with_author <chr>,
#> #   WF_scientific_name_without_author <chr>,
#> #   GL_scientific_name_without_author <chr>,
#> #   SF_scientific_name_without_author <chr>
wamei_check(x2,       hub_master, jn_master, wide=FALSE, ds=c(GL, SF, WF))
#> # A tibble: 32 x 12
#>    input      n_match hub_plus     status  source ID     Family_ID Family_name  
#>    <chr>      <chr>   <chr>        <chr>   <chr>  <chr>  <chr>     <chr>        
#>  1 だみー     0       該当なし     該当なし~ -      -      -         -            
#>  2 ススキ     1       ススキ       確定    GL     GL_03~ 166       Poaceae      
#>  3 ススキ     1       ススキ       確定    WF     WF_02~ 166       Poaceae      
#>  4 ハリガネワラビ~ 1       ハリガネワラビ~ 確定    GL     GL_00~ 42        Thelypterida~
#>  5 ハリガネワラビ~ 1       ハリガネワラビ~ 確定    SF     SF_00~ 42        Thelypterida~
#>  6 オミナエシ 1       オミナエシ   確定    GL     GL_09~ 472       Caprifoliace~
#>  7 オミナエシ 1       オミナエシ   確定    WF     WF_08~ 472       Caprifoliace~
#>  8 コナスビ   1       コナスビ狭義 確定    GL     GL_06~ 398       Primulaceae  
#>  9 コナスビ   1       コナスビ狭義 確定    WF     WF_05~ 398       Primulaceae  
#> 10 カナビキソウ~ 1       カナビキソウ 確定    GL     GL_06~ 339       Santalaceae  
#> # ... with 22 more rows, and 4 more variables: Family_name_JP <chr>,
#> #   common_name <chr>, scientific_name_with_author <chr>,
#> #   scientific_name_without_author <chr>
```

エクセルと同等の出力の場合．

``` r
wamei_check_ex(x1[1:50], hub_master, jn_master, wide=FALSE)
#> # A tibble: 111 x 12
#>    input    n_match Hub_name        status   source ID    Family_ID Family_name 
#>    <chr>      <dbl> <chr>           <chr>    <chr>  <chr> <chr>     <chr>       
#>  1 だみーの和名~       0 ！候補なし      ！個別に検討~ <NA>   <NA>  <NA>      <NA>        
#>  2 mulberry       1 マグワ          確定     WF     WF_0~ 213       Moraceae    
#>  3 mulberry       1 マグワ          確定     YL     YL_0~ 213       Moraceae    
#>  4 no named       0 ！候補なし      ！個別に検討~ <NA>   <NA>  <NA>      <NA>        
#>  5 no_name~       1 no_named_GL01   確定     GL     GL_0~ 101       Potamogeton~
#>  6 no_name~       1 ムカゴサイシンモドキ/no ~ 確定     GL     GL_0~ 124       Orchidaceae 
#>  7 no_name~       1 ムカゴサイシンモドキ/no ~ 確定     WF     WF_0~ 124       Orchidaceae 
#>  8 no_name~       1 ムカゴサイシンモドキ/no ~ 確定     YL     YL_0~ 124       Orchidaceae 
#>  9 no_name~       1 ミチノクサナギイチゴ/no ~ 確定     GL     GL_0~ 206       Rosaceae    
#> 10 no_name~       1 ミチノクサナギイチゴ/no ~ 確定     WF     WF_0~ 206       Rosaceae    
#> # ... with 101 more rows, and 4 more variables: Family_name_JP <chr>,
#> #   common_name <chr>, scientific_name_with_author <chr>,
#> #   scientific_name_without_author <chr>
wamei_check_ex(x2,       hub_master, jn_master)
#> # A tibble: 17 x 23
#>    input   n_match Hub_name    status Family_ID Family_name Family_name_JP GL_ID
#>    <chr>     <dbl> <chr>       <chr>  <chr>     <chr>       <chr>          <chr>
#>  1 だみー        0 ！候補なし  ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#>  2 ススキ        1 ススキ      確定   166       Poaceae     イネ           GL_0~
#>  3 ハリガネワラ~       1 ハリガネワラビ~ 確定   42        Thelypteri~ ヒメシダ       GL_0~
#>  4 オミナエシ~       1 オミナエシ  確定   472       Caprifolia~ スイカズラ     GL_0~
#>  5 コナスビ~       2 コナスビ広義/狭義~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#>  6 カナビキソウ~       1 カナビキソウ~ 確定   339       Santalaceae ビャクダン     GL_0~
#>  7 ヤイトバナ~       1 ヘクソカズラ~ 確定   415       Rubiaceae   アカネ         GL_0~
#>  8 チガヤ        2 チガヤ広義/狭義~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#>  9 キジムシロ~       2 キジムシロ広義/狭義~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#> 10 ハエドクソウ~       3 要検討（ハエドクソウ~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#> 11 キツネノマゴ~       2 キツネノマゴ広義/狭~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#> 12 シロヨメナ~       2 要検討（シロヨメナ/~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#> 13 オオフジシダ~       2 要検討（アイフジシダ~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#> 14 コマツナギ~       2 要検討（コマツナギ/~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#> 15 アイヌタチツ~       2 アイヌタチツボスミレ~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#> 16 シベリアカラ~       2 シベリアカラマツ（マ~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#> 17 アオイモドキ~       2 アオイゴケモドキ/エ~ ！個別に検~ <NA>      <NA>        <NA>           <NA> 
#> # ... with 15 more variables: WF_ID <chr>, YL_ID <chr>, SF_ID <chr>,
#> #   GL_common_name <chr>, WF_common_name <chr>, YL_common_name <chr>,
#> #   SF_common_name <chr>, GL_scientific_name_with_author <chr>,
#> #   WF_scientific_name_with_author <chr>, YL_scientific_name_with_author <chr>,
#> #   SF_scientific_name_with_author <chr>,
#> #   GL_scientific_name_without_author <chr>,
#> #   WF_scientific_name_without_author <chr>,
#> #   YL_scientific_name_without_author <chr>,
#> #   SF_scientific_name_without_author <chr>
```

合致する全ての和名・学名等を出力する場合は，wameicheckrの関数を使わなくても可能．

``` r
hub_long <- 
  hub_master %>%
  tidyr::pivot_longer(cols= GL:YL, names_to = "source", values_to = "ID", values_drop_na = TRUE)
tibble::tibble(input = x2) %>%
  left_join(hub_long, by=c("input"="all_name")) %>%
  left_join(jn_master)
#> Joining, by = c("Family_ID", "Family_name", "Family_name_JP", "ID")
#> # A tibble: 98 x 17
#>    input    Hub_name   lato_stricto Family_ID Family_name  Family_name_JP status
#>    <chr>    <chr>      <chr>        <chr>     <chr>        <chr>          <chr> 
#>  1 だみー   <NA>       <NA>         <NA>      <NA>         <NA>           <NA>  
#>  2 ススキ   ススキ     <NA>         166       Poaceae      イネ           確定  
#>  3 ススキ   ススキ     <NA>         166       Poaceae      イネ           確定  
#>  4 ススキ   ススキ     <NA>         166       Poaceae      イネ           確定  
#>  5 ハリガネワラビ~ ハリガネワラビ~ <NA>         42        Thelypterid~ ヒメシダ       確定  
#>  6 ハリガネワラビ~ ハリガネワラビ~ <NA>         42        Thelypterid~ ヒメシダ       確定  
#>  7 ハリガネワラビ~ ハリガネワラビ~ <NA>         42        Thelypterid~ ヒメシダ       確定  
#>  8 オミナエシ~ オミナエシ <NA>         472       Caprifoliac~ スイカズラ     確定  
#>  9 オミナエシ~ オミナエシ <NA>         472       Caprifoliac~ スイカズラ     確定  
#> 10 オミナエシ~ オミナエシ <NA>         472       Caprifoliac~ スイカズラ     確定  
#> # ... with 88 more rows, and 10 more variables: message <chr>, source <chr>,
#> #   ID <chr>, common_name <chr>, another_name <chr>, another_name_ID <dbl>,
#> #   note_1 <chr>, note_2 <chr>, scientific_name_with_author <chr>,
#> #   scientific_name_without_author <chr>
```

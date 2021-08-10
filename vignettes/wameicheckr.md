```r
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.2     v dplyr   1.0.7
#> v tidyr   1.1.3     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(readxl)
library(wameicheckr)
options(encoding="UTF-8")
```

維管束植物和名チェックリストはパッケージ内にある．
読み込んだあとで，使いやすくするために，若干の整理を実行

```r
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
#>  1 アスヒカ~  アスヒカ~  <NA>         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  2 イヌヤチ~  イヌヤチ~  <NA>         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  3 イワヒモ   ヒモラン   <NA>         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  4 ウチワマ~  マンネン~  <NA>         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  5 エゾコス~  エゾコス~  <NA>         1         Lycopodiac~ ヒカゲノカズラ <NA> 
#>  6 エゾノコ~  コスギラン 広義         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  7 エゾヒカ~  ヒカゲノ~  広義         1         Lycopodiac~ ヒカゲノカズラ <NA> 
#>  8 エゾヒカ~  エゾヒカ~  <NA>         1         Lycopodiac~ ヒカゲノカズラ GL_0~
#>  9 オオスギ~  オオスギ~  <NA>         1         Lycopodiac~ ヒカゲノカズラ <NA> 
#> 10 オニトウ~  オニトウ~  <NA>         1         Lycopodiac~ ヒカゲノカズラ <NA> 
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
#>  3 GL_00002 1         Lycopodiaceae ヒカゲノカズラ コスギラン     エゾノコスギ~ 
#>  4 GL_00002 1         Lycopodiaceae ヒカゲノカズラ コスギラン     チシマスギラン
#>  5 GL_00003 1         Lycopodiaceae ヒカゲノカズラ トウゲシバ     トウゲシバ    
#>  6 GL_00003 1         Lycopodiaceae ヒカゲノカズラ トウゲシバ     ホソバトウゲ~ 
#>  7 GL_00003 1         Lycopodiaceae ヒカゲノカズラ トウゲシバ     ヒロハトウゲ~ 
#>  8 GL_00003 1         Lycopodiaceae ヒカゲノカズラ トウゲシバ     オニトウゲシバ
#>  9 GL_00004 1         Lycopodiaceae ヒカゲノカズラ コスギトウゲ~  コスギトウゲ~ 
#> 10 GL_00005 1         Lycopodiaceae ヒカゲノカズラ イヌヤチスギ~  イヌヤチスギ~ 
#> # ... with 53,212 more rows, and 5 more variables: another_name_ID <dbl>,
#> #   note_1 <chr>, note_2 <chr>, scientific_name_with_author <chr>,
#> #   scientific_name_without_author <chr>
```


最新版が異なる場合は，ウェブページからダウンロードして使用することも可能．

```r
  # Download wamei chek list form https://www.gbif.jp/v2/activities/wamei_checklist.html
  # Change file name (version)
  # Set your directory by setwd()

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

```r
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
多いので，下記では最初の50だけ．
x2は，色々なパターンを含む和名．

```r
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

  # 入力和名に対する和名・学名の候補を出力
  # x1は多いので，最初の50だけ
wamei_check(x1[1:50], hub_master, jn_master)
#> # A tibble: 50 x 28
#>    input    n_match hub_plus         status  source ID    Family_ID Family_name 
#>    <chr>    <chr>   <chr>            <chr>   <chr>  <chr> <chr>     <chr>       
#>  1 だみー~  0       該当なし         該当な~ -      -     -         -           
#>  2 mulberry 1       マグワ           確定    -      -     213       Moraceae    
#>  3 no named 0       該当なし         該当な~ -      -     -         -           
#>  4 no_name~ 1       no_named_GL01    確定    -      -     101       Potamogeton~
#>  5 no_name~ 1       ムカゴサイシン~  確定    -      -     124       Orchidaceae 
#>  6 no_name~ 1       ミチノクサナギ~  確定    -      -     206       Rosaceae    
#>  7 no_name~ 1       ヤエガワカンバ~  確定    -      -     221       Betulaceae  
#>  8 no_name~ 1       no_named_GL05    確定    -      -     221       Betulaceae  
#>  9 no_name~ 1       no_named_GL06    確定    -      -     416       Gentianaceae
#> 10 no_name~ 1       no_named_GL07    確定    -      -     466       Asteraceae  
#> # ... with 40 more rows, and 20 more variables: Family_name_JP <chr>,
#> #   common_name <chr>, scientific_name_with_author <chr>,
#> #   scientific_name_without_author <chr>, WF_ID <chr>, YL_ID <chr>,
#> #   GL_ID <chr>, SF_ID <chr>, WF_common_name <chr>, YL_common_name <chr>,
#> #   GL_common_name <chr>, SF_common_name <chr>,
#> #   WF_scientific_name_with_author <chr>, YL_scientific_name_with_author <chr>,
#> #   GL_scientific_name_with_author <chr>, SF_scientific_name_with_author <chr>,
#> #   WF_scientific_name_without_author <chr>,
#> #   YL_scientific_name_without_author <chr>,
#> #   GL_scientific_name_without_author <chr>,
#> #   SF_scientific_name_without_author <chr>
wamei_check(x1[1:50], hub_master, jn_master, wide=FALSE)
#> # A tibble: 109 x 12
#>    input    n_match hub_plus        status  source ID     Family_ID Family_name 
#>    <chr>    <chr>   <chr>           <chr>   <chr>  <chr>  <chr>     <chr>       
#>  1 だみー~  0       該当なし        該当な~ -      -      -         -           
#>  2 mulberry 1       マグワ          確定    WF     WF_03~ 213       Moraceae    
#>  3 mulberry 1       マグワ          確定    YL     YL_09~ 213       Moraceae    
#>  4 no named 0       該当なし        該当な~ -      -      -         -           
#>  5 no_name~ 1       no_named_GL01   確定    GL     GL_01~ 101       Potamogeton~
#>  6 no_name~ 1       ムカゴサイシン~ 確定    GL     GL_02~ 124       Orchidaceae 
#>  7 no_name~ 1       ムカゴサイシン~ 確定    WF     WF_00~ 124       Orchidaceae 
#>  8 no_name~ 1       ムカゴサイシン~ 確定    YL     YL_03~ 124       Orchidaceae 
#>  9 no_name~ 1       ミチノクサナギ~ 確定    GL     GL_04~ 206       Rosaceae    
#> 10 no_name~ 1       ミチノクサナギ~ 確定    WF     WF_03~ 206       Rosaceae    
#> # ... with 99 more rows, and 4 more variables: Family_name_JP <chr>,
#> #   common_name <chr>, scientific_name_with_author <chr>,
#> #   scientific_name_without_author <chr>
wamei_check(x2,       hub_master, jn_master,             ds=c(GL, SF, WF))
#> # A tibble: 17 x 19
#>    input   n_match hub_plus    status Family_ID Family_name Family_name_JP WF_ID
#>    <chr>   <chr>   <chr>       <chr>  <chr>     <chr>       <chr>          <chr>
#>  1 だみー  0       該当なし    該当~  -         -           -              -    
#>  2 ススキ  1       ススキ      確定   166       Poaceae     イネ           WF_0~
#>  3 ハリガ~ 1       ハリガネワ~ 確定   42        Thelypteri~ ヒメシダ       -    
#>  4 オミナ~ 1       オミナエシ  確定   472       Caprifolia~ スイカズラ     WF_0~
#>  5 コナス~ 1       コナスビ狭~ 確定   398       Primulaceae サクラソウ     WF_0~
#>  6 カナビ~ 1       カナビキソ~ 確定   339       Santalaceae ビャクダン     WF_0~
#>  7 ヤイト~ 1       ヘクソカズ~ 確定   415       Rubiaceae   アカネ         WF_0~
#>  8 チガヤ  1       チガヤ狭義  確定   166       Poaceae     イネ           WF_0~
#>  9 キジム~ 2       キジムシロ~ 確定   206       Rosaceae    バラ           WF_0~
#> 10 ハエド~ 2       ハエドクソ~ ！未~  448       Phrymaceae  ハエドクソウ   WF_0~
#> 11 キツネ~ 1       キツネノマ~ ！未~  440       Acanthaceae キツネノマゴ   WF_0~
#> 12 シロヨ~ 2       シロヨメナ~ ！未~  466       Asteraceae  キク           WF_0~
#> 13 オオフ~ 2       オオフジシ~ ！未~  31        Dennstaedt~ コバノイシカ~  -    
#> 14 コマツ~ 2       コマツナギ~ ！未~  203       Fabaceae    マメ           WF_0~
#> 15 アイヌ~ 2       アイヌタチ~ 確定   263       Violaceae   スミレ         WF_0~
#> 16 シベリ~ 1       シベリアカ~ 確定   174       Ranunculac~ キンポウゲ     WF_0~
#> 17 アオイ~ 1       エノキアオ~ 確定   310       Malvaceae   アオイ         WF_0~
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
#>  1 だみー     0       該当なし     該当な~ -      -      -         -            
#>  2 ススキ     1       ススキ       確定    GL     GL_03~ 166       Poaceae      
#>  3 ススキ     1       ススキ       確定    WF     WF_02~ 166       Poaceae      
#>  4 ハリガネ~  1       ハリガネワ~  確定    GL     GL_00~ 42        Thelypterida~
#>  5 ハリガネ~  1       ハリガネワ~  確定    SF     SF_00~ 42        Thelypterida~
#>  6 オミナエシ 1       オミナエシ   確定    GL     GL_09~ 472       Caprifoliace~
#>  7 オミナエシ 1       オミナエシ   確定    WF     WF_08~ 472       Caprifoliace~
#>  8 コナスビ   1       コナスビ狭義 確定    GL     GL_06~ 398       Primulaceae  
#>  9 コナスビ   1       コナスビ狭義 確定    WF     WF_05~ 398       Primulaceae  
#> 10 カナビキ~  1       カナビキソウ 確定    GL     GL_06~ 339       Santalaceae  
#> # ... with 22 more rows, and 4 more variables: Family_name_JP <chr>,
#> #   common_name <chr>, scientific_name_with_author <chr>,
#> #   scientific_name_without_author <chr>

  # エクセル形式と同等の出力
wamei_check_ex(x1[1:50], hub_master, jn_master, wide=FALSE)
#> # A tibble: 109 x 12
#>    input    n_match Hub_name        status   source ID    Family_ID Family_name 
#>    <chr>      <dbl> <chr>           <chr>    <chr>  <chr> <chr>     <chr>       
#>  1 だみー~        0 ！候補なし      ！個別~  <NA>   <NA>  <NA>      <NA>        
#>  2 mulberry       1 マグワ          確定     WF     WF_0~ 213       Moraceae    
#>  3 mulberry       1 マグワ          確定     YL     YL_0~ 213       Moraceae    
#>  4 no named       0 ！候補なし      ！個別~  <NA>   <NA>  <NA>      <NA>        
#>  5 no_name~       1 no_named_GL01   確定     GL     GL_0~ 101       Potamogeton~
#>  6 no_name~       1 ムカゴサイシン~ 確定     GL     GL_0~ 124       Orchidaceae 
#>  7 no_name~       1 ムカゴサイシン~ 確定     WF     WF_0~ 124       Orchidaceae 
#>  8 no_name~       1 ムカゴサイシン~ 確定     YL     YL_0~ 124       Orchidaceae 
#>  9 no_name~       1 ミチノクサナギ~ 確定     GL     GL_0~ 206       Rosaceae    
#> 10 no_name~       1 ミチノクサナギ~ 確定     WF     WF_0~ 206       Rosaceae    
#> # ... with 99 more rows, and 4 more variables: Family_name_JP <chr>,
#> #   common_name <chr>, scientific_name_with_author <chr>,
#> #   scientific_name_without_author <chr>
wamei_check_ex(x2,       hub_master, jn_master)
#> # A tibble: 17 x 23
#>    input   n_match Hub_name    status Family_ID Family_name Family_name_JP GL_ID
#>    <chr>     <dbl> <chr>       <chr>  <chr>     <chr>       <chr>          <chr>
#>  1 だみー        0 ！候補なし  ！個~  <NA>      <NA>        <NA>           <NA> 
#>  2 ススキ        1 ススキ      確定   166       Poaceae     イネ           GL_0~
#>  3 ハリガ~       1 ハリガネワ~ 確定   42        Thelypteri~ ヒメシダ       GL_0~
#>  4 オミナ~       1 オミナエシ  確定   472       Caprifolia~ スイカズラ     GL_0~
#>  5 コナス~       2 コナスビ広~ ！個~  <NA>      <NA>        <NA>           <NA> 
#>  6 カナビ~       1 カナビキソ~ 確定   339       Santalaceae ビャクダン     GL_0~
#>  7 ヤイト~       1 ヘクソカズ~ 確定   415       Rubiaceae   アカネ         GL_0~
#>  8 チガヤ        2 チガヤ広義~ ！個~  <NA>      <NA>        <NA>           <NA> 
#>  9 キジム~       2 キジムシロ~ ！個~  <NA>      <NA>        <NA>           <NA> 
#> 10 ハエド~       3 要検討（ハ~ ！個~  <NA>      <NA>        <NA>           <NA> 
#> 11 キツネ~       2 キツネノマ~ ！個~  <NA>      <NA>        <NA>           <NA> 
#> 12 シロヨ~       2 要検討（シ~ ！個~  <NA>      <NA>        <NA>           <NA> 
#> 13 オオフ~       2 要検討（ア~ ！個~  <NA>      <NA>        <NA>           <NA> 
#> 14 コマツ~       2 要検討（コ~ ！個~  <NA>      <NA>        <NA>           <NA> 
#> 15 アイヌ~       2 アイヌタチ~ ！個~  <NA>      <NA>        <NA>           <NA> 
#> 16 シベリ~       2 シベリアカ~ ！個~  <NA>      <NA>        <NA>           <NA> 
#> 17 アオイ~       2 アオイゴケ~ ！個~  <NA>      <NA>        <NA>           <NA> 
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
=======
和名チェックリスト ver.1.10のエラーの修正．
バージョンアップで，修正される予定．

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

x1は，和名チェックリストにある和名をすべて抽出．
多いので，下記では最初の50だけ． x2は，色々なパターンを含む和名．

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

      # 入力和名に対する和名・学名の候補を出力
      # x1は多いので，最初の50だけ
    wamei_check(x1[1:50], hub_master, jn_master)
    #> # A tibble: 50 x 28
    #>    input    n_match hub_plus         status  source ID    Family_ID Family_name 
    #>    <chr>    <chr>   <chr>            <chr>   <chr>  <chr> <chr>     <chr>       
    #>  1 だみー~  0       該当なし         該当な~ -      -     -         -           
    #>  2 mulberry 1       マグワ           確定    -      -     213       Moraceae    
    #>  3 no named 0       該当なし         該当な~ -      -     -         -           
    #>  4 no_name~ 1       no_named_GL01    確定    -      -     101       Potamogeton~
    #>  5 no_name~ 1       ムカゴサイシン~  確定    -      -     124       Orchidaceae 
    #>  6 no_name~ 1       ミチノクサナギ~  確定    -      -     206       Rosaceae    
    #>  7 no_name~ 1       ヤエガワカンバ~  確定    -      -     221       Betulaceae  
    #>  8 no_name~ 1       no_named_GL05    確定    -      -     221       Betulaceae  
    #>  9 no_name~ 1       no_named_GL06    確定    -      -     416       Gentianaceae
    #> 10 no_name~ 1       no_named_GL07    確定    -      -     466       Asteraceae  
    #> # ... with 40 more rows, and 20 more variables: Family_name_JP <chr>,
    #> #   common_name <chr>, scientific_name_with_author <chr>,
    #> #   scientific_name_without_author <chr>, WF_ID <chr>, YL_ID <chr>,
    #> #   GL_ID <chr>, SF_ID <chr>, WF_common_name <chr>, YL_common_name <chr>,
    #> #   GL_common_name <chr>, SF_common_name <chr>,
    #> #   WF_scientific_name_with_author <chr>, YL_scientific_name_with_author <chr>,
    #> #   GL_scientific_name_with_author <chr>, SF_scientific_name_with_author <chr>,
    #> #   WF_scientific_name_without_author <chr>,
    #> #   YL_scientific_name_without_author <chr>,
    #> #   GL_scientific_name_without_author <chr>,
    #> #   SF_scientific_name_without_author <chr>
    wamei_check(x1[1:50], hub_master, jn_master, wide=FALSE)
    #> # A tibble: 109 x 12
    #>    input    n_match hub_plus        status  source ID     Family_ID Family_name 
    #>    <chr>    <chr>   <chr>           <chr>   <chr>  <chr>  <chr>     <chr>       
    #>  1 だみー~  0       該当なし        該当な~ -      -      -         -           
    #>  2 mulberry 1       マグワ          確定    WF     WF_03~ 213       Moraceae    
    #>  3 mulberry 1       マグワ          確定    YL     YL_09~ 213       Moraceae    
    #>  4 no named 0       該当なし        該当な~ -      -      -         -           
    #>  5 no_name~ 1       no_named_GL01   確定    GL     GL_01~ 101       Potamogeton~
    #>  6 no_name~ 1       ムカゴサイシン~ 確定    GL     GL_02~ 124       Orchidaceae 
    #>  7 no_name~ 1       ムカゴサイシン~ 確定    WF     WF_00~ 124       Orchidaceae 
    #>  8 no_name~ 1       ムカゴサイシン~ 確定    YL     YL_03~ 124       Orchidaceae 
    #>  9 no_name~ 1       ミチノクサナギ~ 確定    GL     GL_04~ 206       Rosaceae    
    #> 10 no_name~ 1       ミチノクサナギ~ 確定    WF     WF_03~ 206       Rosaceae    
    #> # ... with 99 more rows, and 4 more variables: Family_name_JP <chr>,
    #> #   common_name <chr>, scientific_name_with_author <chr>,
    #> #   scientific_name_without_author <chr>
    wamei_check(x2,       hub_master, jn_master,             ds=c(GL, SF, WF))
    #> # A tibble: 17 x 19
    #>    input   n_match hub_plus    status Family_ID Family_name Family_name_JP WF_ID
    #>    <chr>   <chr>   <chr>       <chr>  <chr>     <chr>       <chr>          <chr>
    #>  1 だみー  0       該当なし    該当~  -         -           -              -    
    #>  2 ススキ  1       ススキ      確定   166       Poaceae     イネ           WF_0~
    #>  3 ハリガ~ 1       ハリガネワ~ 確定   42        Thelypteri~ ヒメシダ       -    
    #>  4 オミナ~ 1       オミナエシ  確定   472       Caprifolia~ スイカズラ     WF_0~
    #>  5 コナス~ 1       コナスビ狭~ 確定   398       Primulaceae サクラソウ     WF_0~
    #>  6 カナビ~ 1       カナビキソ~ 確定   339       Santalaceae ビャクダン     WF_0~
    #>  7 ヤイト~ 1       ヘクソカズ~ 確定   415       Rubiaceae   アカネ         WF_0~
    #>  8 チガヤ  1       チガヤ狭義  確定   166       Poaceae     イネ           WF_0~
    #>  9 キジム~ 2       キジムシロ~ 確定   206       Rosaceae    バラ           WF_0~
    #> 10 ハエド~ 2       ハエドクソ~ ！未~  448       Phrymaceae  ハエドクソウ   WF_0~
    #> 11 キツネ~ 1       キツネノマ~ ！未~  440       Acanthaceae キツネノマゴ   WF_0~
    #> 12 シロヨ~ 2       シロヨメナ~ ！未~  466       Asteraceae  キク           WF_0~
    #> 13 オオフ~ 2       オオフジシ~ ！未~  31        Dennstaedt~ コバノイシカ~  -    
    #> 14 コマツ~ 2       コマツナギ~ ！未~  203       Fabaceae    マメ           WF_0~
    #> 15 アイヌ~ 2       アイヌタチ~ 確定   263       Violaceae   スミレ         WF_0~
    #> 16 シベリ~ 1       シベリアカ~ 確定   174       Ranunculac~ キンポウゲ     WF_0~
    #> 17 アオイ~ 1       エノキアオ~ 確定   310       Malvaceae   アオイ         WF_0~
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
    #>  1 だみー     0       該当なし     該当な~ -      -      -         -            
    #>  2 ススキ     1       ススキ       確定    GL     GL_03~ 166       Poaceae      
    #>  3 ススキ     1       ススキ       確定    WF     WF_02~ 166       Poaceae      
    #>  4 ハリガネ~  1       ハリガネワ~  確定    GL     GL_00~ 42        Thelypterida~
    #>  5 ハリガネ~  1       ハリガネワ~  確定    SF     SF_00~ 42        Thelypterida~
    #>  6 オミナエシ 1       オミナエシ   確定    GL     GL_09~ 472       Caprifoliace~
    #>  7 オミナエシ 1       オミナエシ   確定    WF     WF_08~ 472       Caprifoliace~
    #>  8 コナスビ   1       コナスビ狭義 確定    GL     GL_06~ 398       Primulaceae  
    #>  9 コナスビ   1       コナスビ狭義 確定    WF     WF_05~ 398       Primulaceae  
    #> 10 カナビキ~  1       カナビキソウ 確定    GL     GL_06~ 339       Santalaceae  
    #> # ... with 22 more rows, and 4 more variables: Family_name_JP <chr>,
    #> #   common_name <chr>, scientific_name_with_author <chr>,
    #> #   scientific_name_without_author <chr>

      # エクセル形式と同等の出力
    wamei_check_ex(x1[1:50], hub_master, jn_master, wide=FALSE)
    #> # A tibble: 109 x 12
    #>    input    n_match Hub_name        status   source ID    Family_ID Family_name 
    #>    <chr>      <dbl> <chr>           <chr>    <chr>  <chr> <chr>     <chr>       
    #>  1 だみー~        0 ！候補なし      ！個別~  <NA>   <NA>  <NA>      <NA>        
    #>  2 mulberry       1 マグワ          確定     WF     WF_0~ 213       Moraceae    
    #>  3 mulberry       1 マグワ          確定     YL     YL_0~ 213       Moraceae    
    #>  4 no named       0 ！候補なし      ！個別~  <NA>   <NA>  <NA>      <NA>        
    #>  5 no_name~       1 no_named_GL01   確定     GL     GL_0~ 101       Potamogeton~
    #>  6 no_name~       1 ムカゴサイシン~ 確定     GL     GL_0~ 124       Orchidaceae 
    #>  7 no_name~       1 ムカゴサイシン~ 確定     WF     WF_0~ 124       Orchidaceae 
    #>  8 no_name~       1 ムカゴサイシン~ 確定     YL     YL_0~ 124       Orchidaceae 
    #>  9 no_name~       1 ミチノクサナギ~ 確定     GL     GL_0~ 206       Rosaceae    
    #> 10 no_name~       1 ミチノクサナギ~ 確定     WF     WF_0~ 206       Rosaceae    
    #> # ... with 99 more rows, and 4 more variables: Family_name_JP <chr>,
    #> #   common_name <chr>, scientific_name_with_author <chr>,
    #> #   scientific_name_without_author <chr>
    wamei_check_ex(x2,       hub_master, jn_master)
    #> # A tibble: 17 x 23
    #>    input   n_match Hub_name    status Family_ID Family_name Family_name_JP GL_ID
    #>    <chr>     <dbl> <chr>       <chr>  <chr>     <chr>       <chr>          <chr>
    #>  1 だみー        0 ！候補なし  ！個~  <NA>      <NA>        <NA>           <NA> 
    #>  2 ススキ        1 ススキ      確定   166       Poaceae     イネ           GL_0~
    #>  3 ハリガ~       1 ハリガネワ~ 確定   42        Thelypteri~ ヒメシダ       GL_0~
    #>  4 オミナ~       1 オミナエシ  確定   472       Caprifolia~ スイカズラ     GL_0~
    #>  5 コナス~       2 コナスビ広~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #>  6 カナビ~       1 カナビキソ~ 確定   339       Santalaceae ビャクダン     GL_0~
    #>  7 ヤイト~       1 ヘクソカズ~ 確定   415       Rubiaceae   アカネ         GL_0~
    #>  8 チガヤ        2 チガヤ広義~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #>  9 キジム~       2 キジムシロ~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #> 10 ハエド~       3 要検討（ハ~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #> 11 キツネ~       2 キツネノマ~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #> 12 シロヨ~       2 要検討（シ~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #> 13 オオフ~       2 要検討（ア~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #> 14 コマツ~       2 要検討（コ~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #> 15 アイヌ~       2 アイヌタチ~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #> 16 シベリ~       2 シベリアカ~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #> 17 アオイ~       2 アオイゴケ~ ！個~  <NA>      <NA>        <NA>           <NA> 
    #> # ... with 15 more variables: WF_ID <chr>, YL_ID <chr>, SF_ID <chr>,
    #> #   GL_common_name <chr>, WF_common_name <chr>, YL_common_name <chr>,
    #> #   SF_common_name <chr>, GL_scientific_name_with_author <chr>,
    #> #   WF_scientific_name_with_author <chr>, YL_scientific_name_with_author <chr>,
    #> #   SF_scientific_name_with_author <chr>,
    #> #   GL_scientific_name_without_author <chr>,
    #> #   WF_scientific_name_without_author <chr>,
    #> #   YL_scientific_name_without_author <chr>,
    #> #   SF_scientific_name_without_author <chr>

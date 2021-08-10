## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(wameicheckr)
options(encoding="UTF-8")

## ----read data----------------------------------------------------------------
data(hub_master)
data(jn_master)
hub_master <- 
  hub_master %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~str_replace_all(., "[()]", "")) %>%
  print()

jn_master <- 
  jn_master %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~str_replace_all(., "[()]", "")) %>%
  fill_another_name_id() %>% # another_name_id の空欄を埋める
  print()

## ----input file, eval = FALSE-------------------------------------------------
#    # Download wamei chek list form https://www.gbif.jp/v2/activities/wamei_checklist.html
#    # Change file name (version)
#    # Set your directory by setwd()
#  
#  path <- "wamei_checklist_ver.1.10.xlsx"
#  
#  hub_master <-
#    readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>%
#    dplyr::rename_with(~str_replace_all(., "[ /]", "_")) %>%
#    dplyr::rename_with(~str_replace_all(., "[()]", ""))
#  
#  jn_master <-
#    readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>%
#    dplyr::rename_with(~str_replace_all(., "[ /]", "_")) %>%
#    dplyr::rename_with(~str_replace_all(., "[()]", "")) %>%
#    fill_another_name_id()  # another_name_id の空欄を埋める

## ----modify error-------------------------------------------------------------
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

## ----chekc wamei--------------------------------------------------------------
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
wamei_check(x1[1:50], hub_master, jn_master, wide=FALSE)
wamei_check(x2,       hub_master, jn_master,             ds=c(GL, SF, WF))
wamei_check(x2,       hub_master, jn_master, wide=FALSE, ds=c(GL, SF, WF))

  # エクセル形式と同等の出力
wamei_check_ex(x1[1:50], hub_master, jn_master, wide=FALSE)
wamei_check_ex(x2,       hub_master, jn_master)


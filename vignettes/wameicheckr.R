## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyverse)
library(wameicheckr)
library(magrittr)

## ----read data----------------------------------------------------------------
data(hub_master)
data(jn_master)
hub_master <- 
  hub_master %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~stringr::str_remove_all(., "[()]")) %>%
  print()

jn_master <- 
  jn_master %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~stringr::str_remove_all(., "[()]")) %>%
  fill_another_name_id() %>% # another_name_id の空欄を埋める
  print()

## ----input file, eval = FALSE-------------------------------------------------
#    # Download wamei chek list form https://www.gbif.jp/v2/activities/wamei_checklist.html
#    # Change file name (version)
#    # Set your directory by setwd()
#  library(readxl)
#  
#  path <- "wamei_checklist_ver.1.10.xlsx"
#  
#  hub_master <-
#    readxl::read_xlsx(path, sheet="Hub_data",   col_types="text") %>%
#    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
#    dplyr::rename_with(~stringr::str_remove_all(., "[()]"))
#  
#  jn_master <-
#    readxl::read_xlsx(path, sheet="JN_dataset", col_types="text") %>%
#    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
#    dplyr::rename_with(~stringr::str_remove_all(., "[()]")) %>%
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

## ----prep wamei---------------------------------------------------------------
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

## ----check wamei--------------------------------------------------------------
wamei_check(x1[1:50], hub_master, jn_master)
wamei_check(x1[1:50], hub_master, jn_master, wide=FALSE)
wamei_check(x2,       hub_master, jn_master,             ds=c(GL, SF, WF))
wamei_check(x2,       hub_master, jn_master, wide=FALSE, ds=c(GL, SF, WF))

## ----check wamei_ex-----------------------------------------------------------
wamei_check_ex(x1[1:50], hub_master, jn_master, wide=FALSE)
wamei_check_ex(x2,       hub_master, jn_master)

## ----get all info-------------------------------------------------------------
hub_long <- 
  hub_master %>%
  tidyr::pivot_longer(cols= GL:YL, names_to = "source", values_to = "ID", values_drop_na = TRUE)
tibble::tibble(input = x2) %>%
  left_join(hub_long, by=c("input"="all_name")) %>%
  left_join(jn_master)


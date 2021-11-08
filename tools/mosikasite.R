library(tidyverse)
library(wameicheckr)
library(magrittr)
data(hub_master)
data(jn_master)
hub_master <- 
  hub_master %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~stringr::str_replace_all(., "[()]", "")) %>%
  print()
jn_master <- 
  jn_master %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
  dplyr::rename_with(~stringr::str_replace_all(., "[()]", "")) %>%
  fill_another_name_id() %>% # another_name_id の空欄を埋める
  print()
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
  # hubやjnの種名を抽出(全種)
x1 <- 
  c(hub_master$all_name, hub_master$Hub_name, jn_master$common_name, jn_master$another_name) %>%
  purrr::map(str_split, "/") %>%
  unlist() %>% unique() %>% sort()
  # 全角にして，UTF8のエスケープ文字に
x1 <- 
  x1 %>% 
  stri_trans_general("halfwidth-fullwidth") %>%
  stri_escape_unicode()
tibble(x1=x1) %>%
  mutate(len = str_length(x1) %% 6) %>%
  filter(len != 0)

str2strvec()



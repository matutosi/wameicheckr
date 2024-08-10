  #' Prepare data in wameicheckr package
  #' 
  #' dowonload excel sheet from https://www.gbif.jp/v2/activities/wamei_checklist.html
  #' @param path Character vector of a file name of wamei_checklist.
  #' 
  #' @examples
  #' # path <- "d:/wamei_checklist_ver.1.10.xlsx"
  #' # prep_data_all(path)
  #' # prep_hub_data(path)
  #' # prep_jn_data(path)
  #' # prep_ref_data()
prep_data_all <- function(path){
  prep_hub_data(path)
  prep_jn_data(path)
  prep_ref_data()
}

  #' @describeIn prep_data_all Prepare hub data
prep_hub_data <- function(path){
  hub_master <- readxl::read_xlsx(path, sheet="Hub_data",   col_types="text")
  usethis::use_data(hub_master, overwrite=TRUE)
}

  #' @describeIn prep_data_all Prepare jn data
prep_jn_data <- function(path){
  jn_master  <- readxl::read_xlsx(path, sheet="JN_dataset", col_types="text")
  usethis::use_data(jn_master, overwrite=TRUE)
}

  #' @describeIn prep_data_all Read hub and jn data
read_hub_jn <- function(){
  data(hub_master)
  data(jn_master)

  hub_master <- 
    hub_master %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_remove_all(., "[()]"))
  jn_master <- 
    jn_master %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_remove_all(., "[()]")) %>%
    fill_another_name_id()

  no_id_0 <- 
    c("SF_00131", "SF_00323", "WF_01542", "WF_02219", "WF_04287", "SF_00127","WF_01902","WF_03825","YL_11456","YL_17759")
  jn_master$another_name_ID[jn_master$ID %in% no_id_0 & jn_master$another_name_ID != 0] <- 0

  hub_master$Hub_name[
    hub_master$Hub_name==stringi::stri_unescape_unicode("\\u30b7\\u30d9\\u30ea\\u30a2\\u30ab\\u30e9\\u30de\\u30c4") &
    hub_master$Family_name_JP==stringi::stri_unescape_unicode("\\u30de\\u30c4")] <- 
    stringi::stri_unescape_unicode("\\u30b7\\u30d9\\u30ea\\u30a2\\u30ab\\u30e9\\u30de\\u30c4(\\u30de\\u30c4\\u79d1)")
  hub_master$Hub_name[
    hub_master$Hub_name=="\\u30b7\\u30d9\\u30ea\\u30a2\\u30ab\\u30e9\\u30de\\u30c4" &
    hub_master$Family_name_JP==stringi::stri_unescape_unicode("\\u30ad\\u30f3\\u30dd\\u30a6\\u30b2")] <- 
    stringi::stri_unescape_unicode("\\u30b7\\u30d9\\u30ea\\u30a2\\u30ab\\u30e9\\u30de\\u30c4(\\u30ad\\u30f3\\u30dd\\u30a6\\u30b2\\u79d1)")
  jn_master$Family_name_JP[
    jn_master$Family_name_JP==stringi::stri_unescape_unicode("\\u30c4\\u30eb\\u30dc\\u30e9\\u30f3")] <- 
    stringi::stri_unescape_unicode("ワスレグサ") %>%
  list(hub_master=hub_master, jn_master=jn_master)
}

  #' @describeIn prep_data_all Prepare reference data
prep_ref_data <- function(){
  # save ref_jp and ref_sc

  hub_jn <- read_hub_jn()
  hub_master <- hub_jn$hub_master
  jn_master  <- hub_jn$jn_master

  # # # # # ref_jp # # # # # 
  ref_jp <- 
    jn_master %>%
    dplyr::select(ID, common_name, another_name) %>%
    tidyr::separate(ID, c("source", NA), "_") %>%
    tidyr::pivot_longer(c(common_name, another_name), names_to="div", values_to="name_jp") %>%
    dplyr::select(source, name_jp) %>%
    dplyr::distinct()

  ref_jp <- 
    ref_jp %>%
    dplyr::mutate(name_jp = stringi::stri_trans_general(name_jp, "halfwidth-fullwidth")) %>%
    dplyr::mutate(name_jp = stringi::stri_escape_unicode(name_jp))

  usethis::use_data(ref_jp, overwrite=TRUE)
  # tibble::tibble(ref_jp)
  # stringi::stri_unescape_unicode(ref_jp) %>% tibble::tibble()
  # setdiff(c(hub_master$all_name, hub_master$Hub_name), c(jn_master$common_name, jn_master$another_name))
  # setdiff(c(jn_master$common_name, jn_master$another_name), c(hub_master$all_name, hub_master$Hub_name))

  # # # # # ref_sc # # # # # 
  ref_sc <- 
    jn_master %>%
    dplyr::select(ID, scientific_name_with_author, scientific_name_without_author) %>%
    tidyr::separate(ID, c("source", NA), "_") %>%
    tidyr::pivot_longer(c(scientific_name_with_author, scientific_name_without_author), names_to="div", values_to="name_sc") %>%
    dplyr::select(source, name_sc) %>%
    dplyr::distinct()

  usethis::use_data(ref_sc, overwrite=TRUE)
}

  #' another_name_IDの空欄を埋める
  #' 
  #' jn_masterのanother_name_IDが空欄の場合，common_nameごとで五十音順で0から
  #' 順番にIDを付与する．
  #' 
  #' @param jn_master 和名チェックリストの「JN_dataset」シートのデータ
  #' 
  #' @return tibble．another_name_IDの空欄を埋めたjn_master
  #' 
  #' @examples
  #' jn_master %>%
  #'   tibble::as_tibble() %>%
  #'   dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
  #'   dplyr::rename_with(~stringr::str_replace_all(., "[()]", "")) %>%
  #'   fill_another_name_id()
  #' 
  #' @export
fill_another_name_id <- function(jn_master){
  # fill_another_name_id がOKのもの
  an_id_ok <- 
    jn_master %>%
    tibble::rownames_to_column("row_num") %>%
    dplyr::mutate(another_name_ID=as.numeric(another_name_ID)) %>%
    dplyr::filter(! is.na(another_name_ID))
  # fill_another_name_id がNGのもの
  an_id_ng <- 
    jn_master %>%
    tibble::rownames_to_column("row_num") %>%
    dplyr::filter(is.na(another_name_ID)) %>%
    dplyr::mutate(ID2=dplyr::lag(ID, default="")) %>%
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

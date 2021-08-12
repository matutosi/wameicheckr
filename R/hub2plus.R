  #' hub_plusを生成する
  #' 
  #' hub_masterのhub_nameとlato_strictoから，hub_plusを生成．
  #' 「/」での区切りがある場合は，前後両方に「lato_stricto」を追加
  #' 
  #' @param hub_name hub_name in hub_master
  #' @param lato_stricto lato_stricto in hub_master
  #' 
  #' @return string
  #' 
  #' @examples
  #' Hub_master %>%
  #'  dplyr::mutate(hub_plus = hub2plus(Hub_name, lato_stricto))
hub2plus <- function(Hub_name, lato_stricto){
  hub_name <- 
    Hub_name %>% 
    stringr::str_split("/")
  purrr::map2(hub_name, lato_stricto, paste, sep="-", collapse="/") %>%
    stringr::str_replace_all("-NA", "")
}

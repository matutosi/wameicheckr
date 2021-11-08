  #' /を含むmessage・和名の整理
  #' 
  #' 複数の和名(hub_plus)がある場合，重複のないように整理．
  #' 広義と狭義の両方があるときは，「/」区切りで出力．
  #' wamei_check.R ではmessageだが，実質は和名の一覧．
  #' 
  #' @param x 整理する和名
  #' 
  #' @return string 和名の候補一覧の「，」(全角)区切り．
  #' カタカナ部分が共通で広義と狭義ともある場合は「ワメイ広義/狭義」．
  #' 
  #' @examples
  #' library(stringi)
  #' x <- stringi::stri_unescape_unicode("\\u30ef\\u30e1\\u30a4/\\u5e83\\u7fa9\\uff0c\\u30ef\\u30e1\\u30a4/\\u72ed\\u7fa9")
  #' arrange_hub_name(x)
  #' 
  #' @export
arrange_hub_name <- function(x){
  # 入力が空の場合
  if(! is.character(x)) return("")
  x <-
    x %>%
    stringr::str_split("，|；|/") %>%
    purrr::map(sort) %>%
    purrr::map(unique) %>%
    purrr::map(function(x) if(length(x)==0) "" else x)
  # tibbleにしてから，case_whenやreduceなどの処理
  x <-
    x %>%
    purrr::map(~tibble::tibble(hub_plus=.)) %>%
    purrr::map(~tidyr::separate( ., hub_plus, into=c("hub", "plus"), sep="-", fill="right")) %>%
    purrr::map(~dplyr::transmute(., 
      hub_plus = dplyr::case_when( hub == dplyr::lag(hub) ~ plus, TRUE ~ paste(hub, plus, sep="")), 
      hub_plus = purrr::reduce(hub_plus, ~stringr::str_c(.x, "/", .y))    )) %>%
    purrr::map(dplyr::distinct) %>%
    unlist()
  # 細かな修正
  x %>%
    `names<-`(NULL) %>%
    stringr::str_replace_all("NA", "")
}

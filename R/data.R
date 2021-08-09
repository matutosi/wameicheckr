#' Hub_name data from wamei check list
#'
#' A dataset containing wamei (Japanese name) of vascular plant species, 
#'    data source ID, and other attributes. 
#' Check the detail in data by folloing URL. 
#' Wamei checklist (ver. 1.10) is form: 
#'   Yamanouchi, T., Shutoh, K., Osawa, T., Yonekura, K., Kato, S., Shiga, T.
#'     2019. A checklist of Japanese plant names 
#'     (https://www.gbif.jp/v2/activities/wamei_checklist.html)
#' The variables are as follows:
#'
#' @format A data frame with 30430 rows and 12 variables:
#' \describe{
#'   \item{all_name}{all wamei form data source including synonym}
#'   \item{Hub name}{hub name}
#'   \item{lato/stricto}{}
#'   \item{Family ID}{family ID}
#'   \item{Family name}{family name}
#'   \item{Family name(JP)}{family name in Japanese}
#'   \item{GL}{ID in GL}
#'   \item{SF}{ID in SF}
#'   \item{WF}{ID in WF}
#'   \item{YL}{ID in YL}
#'   \item{status}{status}
#'   \item{message}{message}
#' }
"hub_master"

#' jn_name data from wamei check list
#'
#' A dataset containing wamei (Japanese name) of vascular plant species, 
#'    data source ID, family name, common name, scientific name, and 
#'    other attributes.
#' Check the detail in data by folloing URL. 
#' Wamei checklist (ver. 1.10) is form: 
#'   Yamanouchi, T., Shutoh, K., Osawa, T., Yonekura, K., Kato, S., Shiga, T.
#'     2019. A checklist of Japanese plant names 
#'     (https://www.gbif.jp/v2/activities/wamei_checklist.html)
#' The variables are as follows:
#'
#' @format A data frame with 53222 rows and 11 variables:
#' \describe{
#'   \item{ID}{ID of each data source}
#'   \item{Family ID}{family ID}
#'   \item{Family name}{family name}
#'   \item{Family name(JP)}{family name in Japanese}
#'   \item{common name}{common Japanese name}
#'   \item{another name}{another Japanese name}
#'   \item{another name ID}{another name ID}
#'   \item{note 1}{note 1}
#'   \item{note 2}{note 1}
#'   \item{scientific name with author}{scientific name with author}
#'   \item{scientific name without author}{scientific name without author}
#' }
"jn_master"

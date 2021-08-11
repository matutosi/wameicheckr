#' Hub_name data from wamei checklist
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
#' 各項目の説明は，https://www.gbif.jp/v2/activities/wamei_checklist.html から．
#' \describe{
#'   \item{all_name}            {引用元に掲載された、異名を含むすべての和名。}
#'   \item{Hub name}            {データベース上での標準的な和名を示す。原則として各データソースでの標準的な
#'                               和名を採用したが、データソース間で標準的な和名が一致していない場合、それら
#'                               をスラッシュで区切って併記した。}
#'   \item{lato/stricto}        {Hub nameに広義と狭義がある場合、その区分を記した。}
#'   \item{Family ID}           {科名のID。}
#'   \item{Family name}         {科の学名。}
#'   \item{Family name(JP)}     {科の和名。}
#'   \item{GL}                  {「JN_dataset」に採録されたGreen List掲載種の種ID。}
#'   \item{SF}                  {「JN_dataset」に採録されたシダ標準図鑑I～II掲載種の種ID。}
#'   \item{WF}                  {「JN_dataset」に採録された野生植物1～5掲載種の種ID。}
#'   \item{YL}                  {「JN_dataset」に採録されたYlist更新データ掲載種の種ID。}
#'   \item{status}              {各データソース間で、和名の指す分類群の同一性が確認できたものは"確定"とし、
#'                               和名の指す範囲が異なるなど分類学的な検討を要するものは"！未統合"とした。}
#'   \item{message}             {一つのall nameに対し複数のHub nameが相当する、すなわち異物同名が存在する
#'                               場合に、その候補となるすべてのHub nameをスラッシュで区切って併記した。
#'                               例えば、all name「エゾヒカゲノカズラ」はHub name「エゾヒカゲノカズラ」と
#'                               Hub name「ヒカゲノカズラ広義」に該当する。}
#' }
"hub_master"

#' jn_name data from wamei checklist
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
#'   \item{ID}                            {各分類群に与えられた種ID。}
#'   \item{Family ID}                     {科名のID。}
#'   \item{Family name}                   {科の学名。}
#'   \item{Family name(JP)}               {科の和名。}
#'   \item{common name}                   {各データソースでの標準的な和名。}
#'   \item{another name}                  {各データソースに掲載された異名を含むすべての和名。}
#'   \item{another name ID}               {各分類群内の異名に与えられたID。}
#'   \item{note 1}                        {和名の示す範囲に関する備考。}
#'   \item{note 2}                        {データソースから学名を採録するにあたっての修正等。}
#'   \item{scientific name with author}   {author nameを含む学名。}
#'   \item{scientific name without author}{author nameを除く学名。}
#' }
"jn_master"

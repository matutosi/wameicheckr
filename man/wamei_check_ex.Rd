% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wamei_check_ex.R
\name{wamei_check_ex}
\alias{wamei_check_ex}
\title{和名チェックリストから和名・学名の候補を出力}
\usage{
wamei_check_ex(x, hub_master, jn_master, wide = TRUE)
}
\arguments{
\item{x}{検索する和名の文字列のベクトル．}

\item{hub_master}{和名チェックリストの「Hub_data」シートのデータ}

\item{jn_master}{和名チェックリストの「JN_dataset」シートのデータ}

\item{wide}{論理値：出力形式}
}
\value{
tibble形式．検索した和名をキーとして，和名・学名など．
維管束植物和名変換シート(excel版)と同等．
https://wetlands.info/tools/plantsdb/nameconv/
}
\description{
和名チェックリストを使用して，和名・学名の候補を出力．
和名チェックリスト
https://www.gbif.jp/v2/activities/wamei_checklist.html
https://wetlands.info/tools/plantsdb/nameconv/
}
\examples{
# see vignette
# vignette("wamei_checkr")

}
\seealso{
wamei_check()
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill_another_name_id.R
\name{fill_another_name_id}
\alias{fill_another_name_id}
\title{another_name_IDの空欄を埋める}
\usage{
fill_another_name_id(jn_master)
}
\arguments{
\item{jn_master}{和名チェックリストの「JN_dataset」シートのデータ}
}
\value{
tibble．another_name_IDの空欄を埋めたjn_master
}
\description{
jn_masterのanother_name_IDが空欄の場合，common_nameごとで五十音順で0から
順番にIDを付与する．
}
\examples{
jn_master \%>\%
  tibble::as_tibble() \%>\%
  dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) \%>\%
  dplyr::rename_with(~stringr::str_remove_all(., "[()]")) \%>\%
  fill_another_name_id()

}

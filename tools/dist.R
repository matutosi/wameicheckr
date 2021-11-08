  # 
library(Rcpp)
  # library(MiscPsycho)
  # stringMatch
function (string.1, string.2, normalize=TRUE, penalty = 1){
string.1 <- "ちからうどん"
string.2 <- "からげんき"
    string.1 <- as.character(string.1)
    string.2 <- as.character(string.2)
    s1 <- strsplit(string.1, split = "")[[1]]
    s2 <- strsplit(string.2, split = "")[[1]]
    n <- length(s1)
    m <- length(s2)
    d <- matrix(0, nrow = n + 1, ncol = m + 1)
    d[, 1] <- 0:n
    d[1, ] <- 0:m
    d[1, 1] <- 0
    for (i in 2:(n + 1)) {
        for (j in 2:(m + 1)) {
            if (s1[i - 1] == s2[j - 1]) cost <- 0
            else                        cost <- 1
            d[i, j] <- min(d[i - 1, j] + 1, d[i, j - 1] + 1, d[i - 1, j - 1] + cost)
        }
    }
    if(normalize) 1 - d[n + 1, m + 1]/max(n, m) 
    else          d[n + 1, m + 1]
}

library(Rcpp)
sourceCpp("d:/test.cpp")


  # // editdist("kitten", "sitting")
  # // library(Rcpp)
  # // sourceCpp("D:/dropbox/ToDo/wameicheckr/src/editdist.cpp")
  # // sourceCpp("D:/matu/work/ToDo/wameicheckr/tools/text.cpp")
  # // https://teuder.github.io/rcpp4everyone_ja/index.html
  # // https://www.kkaneko.jp/data/r/rcpp.html

  # // library(Rcpp)
  # // sourceCpp("D:/matu/work/ToDo/wameicheckr/tools/distedit.cpp")
  # // sourceCpp("D:/dropbox/ToDo/wameicheckr/tools/distedit.cpp")
  # // https://teuder.github.io/rcpp4everyone_ja/index.html
  # // https://www.kkaneko.jp/data/r/rcpp.html

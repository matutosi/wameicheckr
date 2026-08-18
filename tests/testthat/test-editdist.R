# editdist() は素朴な動的計画法なので，これを正解とみなして
# 高速版 editdist_bp() と editdist_pairs() を突き合わせる．

rand_str <- function(n, alphabet) {
  paste0(sample(alphabet, n, replace = TRUE), collapse = "")
}

kana <- strsplit("\u30a2\u30a4\u30a6\u30a8\u30aa\u30ab\u30ad\u30af\u30b1\u30b3\u30b5\u30b7\u30b9\u30bb\u30bd\u30cf\u30b0\u30b6\u30c5\u30f3\u30fc", "")[[1]]

test_that("editdist_bp() は editdist() と一致する (len = 1)", {
  set.seed(1)
  for (bp_min in c(0L, 1L, 16L, 64L)) {
    for (i in 1:200) {
      s1 <- rand_str(sample(0:40, 1), letters[1:5])
      s2 <- rand_str(sample(0:40, 1), letters[1:5])
      expect_equal(editdist_bp(s1, s2, 1L, bp_min), editdist(s1, s2, 1L))
    }
  }
})

test_that("editdist_bp() は editdist() と一致する (len = 6, 和名)", {
  set.seed(2)
  esc <- stringi::stri_escape_unicode(kana)
  for (i in 1:200) {
    s1 <- paste0(sample(esc, sample(1:14, 1), replace = TRUE), collapse = "")
    s2 <- paste0(sample(esc, sample(1:14, 1), replace = TRUE), collapse = "")
    expect_equal(editdist_bp(s1, s2, 6L), editdist(s1, s2, 6L))
  }
})

test_that("editdist_bp() は 64 トークンの境界をまたいでも一致する", {
  set.seed(3)
  for (i in 1:100) {
    s1 <- rand_str(sample(60:70, 1), letters[1:4])
    s2 <- rand_str(sample(60:140, 1), letters[1:4])
    expect_equal(editdist_bp(s1, s2, 1L), editdist(s1, s2, 1L))
  }
})

test_that("editdist_bp() は空文字と同一文字を扱える", {
  expect_equal(editdist_bp("", "abc"), 3L)
  expect_equal(editdist_bp("abc", ""), 3L)
  expect_equal(editdist_bp("", ""), 0L)
  expect_equal(editdist_bp("abc", "abc"), 0L)
  expect_equal(editdist_bp("kitten", "sitting"), 3L)
})

test_that("editdist_pairs() は expand_grid の順で editdist() と一致する", {
  set.seed(4)
  input     <- replicate(20, rand_str(sample(1:30, 1), letters[1:6]))
  reference <- replicate(20, rand_str(sample(1:30, 1), letters[1:6]))
  expected <- as.integer(unlist(lapply(input, function(s1) {
    vapply(reference, function(s2) editdist(s1, s2, 1L), integer(1))
  })))
  for (bp_min in c(0L, 8L, 16L, 64L)) {
    expect_equal(editdist_pairs(input, reference, 1L, bp_min), expected)
  }
})

test_that("editdist_multi() は編集距離と標準化編集距離を返す", {
  input     <- stringi::stri_unescape_unicode(c("\u30cf\u30c3\u30ab\u30b0\u30b5", "\u30b9\u30ba\u30ce\u30a8\u30f3\u30c9\u30a6"))
  reference <- stringi::stri_unescape_unicode(c("\u30cf\u30b7\u30ab\u30b0\u30b5", "\u30b9\u30ba\u30e1\u30ce\u30a8\u30f3\u30c9\u30a6"))
  res <- editdist_multi(input = input, reference = reference, len = 6)

  expect_equal(nrow(res), 4L)
  expect_named(res, c("s1", "s2", "len", "editdist", "editdist_norm"))
  # 1 文字だけ違う和名
  expect_equal(res$editdist[1], 1L)
  expect_equal(res$editdist_norm[1], 1 / 5)
  # 1 文字だけ挿入された和名
  expect_equal(res$editdist[4], 1L)
  expect_equal(res$editdist_norm[4], 1 / 8)
  # 表示は元の和名に戻っている
  expect_equal(res$s1[1], input[1])
})

test_that("editdist_norm() はベクトルを受け取れる", {
  expect_equal(editdist_norm(c("abcde", "abc"), c("abcd", "xyz"), c(1L, 3L)),
               c(1 / 5, 3 / 3))
})

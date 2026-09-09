# maybe() (学名) と mosiya() (和名) の特性テスト．
# 2 つを 1 つにまとめる予定なので，まとめる前後で振る舞いが変わらないことを
# 確かめるためのもの．どちらも参照データ全体と突き合わせるので，
# 例は少なく，確かめるのは列の並び・絞り込みの条件・結合の結果に絞る．

test_that("maybe() は似た学名の候補を返す", {
  res <- maybe("Viola madahuricaa")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("input", "reference", "editdist", "editdist_norm", "source"))
  expect_true(nrow(res) > 0)
  expect_true(all(res$input == "Viola madahuricaa"))
  expect_true("Viola mandshurica" %in% res$reference)
})

test_that("maybe() は min_dist か min_dist_norm のどちらかを満たす候補だけ返す", {
  res <- maybe("Carex nevarta", min_dist = 4, min_dist_norm = 0.2)
  expect_true(nrow(res) > 0)
  expect_true(all(res$editdist < 4 | res$editdist_norm < 0.2))
  # 条件を狭めれば候補は増えない
  narrow <- maybe("Carex nevarta", min_dist = 2, min_dist_norm = 0.1)
  expect_true(nrow(narrow) <= nrow(res))
})

test_that("maybe() はベクトルを受け取り，入力ごとに候補を返す", {
  x <- c("Viola madahuricaa", "Carex nevarta")
  res <- maybe(x)
  expect_setequal(unique(res$input), x)
})

test_that("maybe() は該当が無ければ 0 行を返す", {
  res <- maybe("Xyzzyq wwwwwwwwwwqqqq", min_dist = 1, min_dist_norm = 0.01)
  expect_equal(nrow(res), 0L)
  expect_named(res, c("input", "reference", "editdist", "editdist_norm", "source"))
})

test_that("mosiya() は似た和名の候補を返す", {
  # ハッカグサ (実在しない和名)
  x <- stringi::stri_unescape_unicode("\u30cf\u30c3\u30ab\u30b0\u30b5")
  res <- mosiya(x)
  expect_named(res, c("input", "reference", "editdist", "editdist_norm", "source"))
  expect_true(nrow(res) > 0)
  # 参照はエスケープを戻した和名で返る
  expect_false(any(grepl("\\u", res$reference)))
  # ハシカグサ (1 文字違い)が候補に入る
  expect_true(stringi::stri_unescape_unicode("\u30cf\u30b7\u30ab\u30b0\u30b5")
              %in% res$reference)
})

test_that("mosiya() の既定の絞り込みは min_dist = 3", {
  x <- stringi::stri_unescape_unicode("\u30cf\u30c3\u30ab\u30b0\u30b5")
  res <- mosiya(x)
  expect_true(all(res$editdist < 3 | res$editdist_norm < 0.2))
})

test_that("mosiya() は和名 1 文字を 1 単位として数える", {
  # 「スズノエンドウ」は「スズメノエンドウ」と 1 文字違い(len = 6 で 1)
  x  <- stringi::stri_unescape_unicode("\u30b9\u30ba\u30ce\u30a8\u30f3\u30c9\u30a6")
  ref <- stringi::stri_unescape_unicode(
    "\u30b9\u30ba\u30e1\u30ce\u30a8\u30f3\u30c9\u30a6")
  res <- mosiya(x)
  hit <- res[res$reference == ref, ]
  expect_true(nrow(hit) > 0)
  expect_equal(unique(hit$editdist), 1L)
})

test_that("maybe() と mosiya() は同じ列を返す", {
  a <- maybe("Viola madahuricaa")
  b <- mosiya(stringi::stri_unescape_unicode("\u30cf\u30c3\u30ab\u30b0\u30b5"))
  expect_equal(names(a), names(b))
})

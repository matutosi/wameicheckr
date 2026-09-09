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

# ここから下は，絞り込みを C++ へ移し mosiya() を maybe() のラッパーにした
# 変更(2026-09-10)で足したもの．

test_that("editdist_close_pairs() は editdist_multi() を絞ったものと一致する", {
  set.seed(11)
  rand <- function(n) replicate(n, paste0(sample(letters[1:6], sample(3:12, 1),
                                                 replace = TRUE), collapse = ""))
  input     <- rand(15)
  reference <- rand(40)
  for (md in c(1, 3, 5)) {
    for (mn in c(0.05, 0.2, 0.5)) {
      expected <- editdist_multi(input, reference, len = 1)
      expected <- expected[expected$editdist < md | expected$editdist_norm < mn, ]
      res <- editdist_close_pairs(input, reference, len = 1,
                                  min_dist = md, min_dist_norm = mn)
      expect_equal(nrow(res), nrow(expected))
      expect_equal(input[res$input_id], expected$s1)
      expect_equal(reference[res$reference_id], expected$s2)
      expect_equal(res$editdist, expected$editdist)
      expect_equal(res$editdist_norm, expected$editdist_norm)
    }
  }
})

test_that("editdist_close_pairs() は len = 6 (和名) でも一致する", {
  set.seed(12)
  kana <- strsplit(stringi::stri_unescape_unicode(
    "\u30a2\u30a4\u30a6\u30a8\u30aa\u30ab\u30ad\u30af\u30b1\u30b3"), "")[[1]]
  kana6 <- stringi::stri_escape_unicode(kana)
  rand <- function(n) replicate(n, paste0(sample(kana6, sample(3:10, 1),
                                                 replace = TRUE), collapse = ""))
  input     <- rand(10)
  reference <- rand(30)
  expected <- editdist_multi(input, reference, inp_esc = TRUE, ref_esc = TRUE, len = 6)
  expected <- expected[expected$editdist < 3 | expected$editdist_norm < 0.2, ]
  res <- editdist_close_pairs(input, reference, len = 6,
                              min_dist = 3, min_dist_norm = 0.2)
  expect_equal(nrow(res), nrow(expected))
  expect_equal(res$editdist, expected$editdist)
  expect_equal(res$editdist_norm, expected$editdist_norm)
})

test_that("editdist_close_pairs() は該当が無ければ 0 行を返す", {
  res <- editdist_close_pairs("abcdefgh", "zzzz", len = 1,
                              min_dist = 1, min_dist_norm = 0.01)
  expect_equal(nrow(res), 0L)
  expect_named(res, c("input_id", "reference_id", "editdist", "editdist_norm"))
})

test_that("mosiya() は maybe() のラッパーである", {
  x <- stringi::stri_unescape_unicode("\u30cf\u30c3\u30ab\u30b0\u30b5")
  expect_equal(mosiya(x), maybe(x, len = 6, min_dist = 3))
  # 既定が違うだけで，同じ引数を渡せば同じ結果
  expect_equal(mosiya(x, min_dist = 2), maybe(x, len = 6, min_dist = 2))
})

test_that("maybe() は len で参照を選ぶ", {
  # len = 6 なら和名の参照(ref_jp)を見る
  x <- stringi::stri_unescape_unicode("\u30cf\u30c3\u30ab\u30b0\u30b5")
  jp <- maybe(x, len = 6, min_dist = 3)
  expect_true(all(jp$reference %in% stringi::stri_unescape_unicode(ref_jp$name_jp)))
  # len = 1 なら学名の参照(ref_sc)を見る
  sc <- maybe("Viola madahuricaa")
  expect_true(all(sc$reference %in% ref_sc$name_sc))
})

test_that("maybe() と mosiya() は多対多の警告を出さない", {
  # 1 つの名前が複数のデータソースに載るので，結合は意図した多対多．
  expect_no_warning(maybe("Carex nevarta"))
  expect_no_warning(mosiya(stringi::stri_unescape_unicode("\u30cf\u30c3\u30ab\u30b0\u30b5")))
})

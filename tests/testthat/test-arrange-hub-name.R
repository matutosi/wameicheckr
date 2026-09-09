# arrange_hub_name() と hub2plus() の特性テスト．
# 両者は hub_master の Hub name と lato/stricto を hub_plus へまとめ，
# 重複のない和名の一覧に整える組み合わせで使う．
# 並べ替えに sort() を使っており，日本語の順序はロケールに依存するので，
# 順序を確かめる例は ASCII だけにする．

test_that("hub2plus() は hub_name と lato_stricto を「-」でつなぐ", {
  expect_equal(hub2plus("A", "a"), "A-a")
  # 「/」区切りがあれば前後の両方に lato_stricto を付ける
  expect_equal(hub2plus("A/B", "a"), "A-a/B-a")
  # lato_stricto が無い(NA)ときは「-NA」を落とす
  expect_equal(hub2plus("A", NA), "A")
  expect_equal(hub2plus("A/B", NA), "A/B")
})

test_that("hub2plus() はベクトルを受け取り character を返す", {
  res <- hub2plus(c("A", "A/B"), c("a", NA))
  expect_type(res, "character")
  expect_equal(res, c("A-a", "A/B"))
})

test_that("arrange_hub_name() は共通の hub をまとめて「/」で並べる", {
  # hub が同じなら 2 件目以降は plus だけを残す
  expect_equal(arrange_hub_name("A-a，A-b"), "Aa/b")
  # hub が違えば hub ごと並べる
  expect_equal(arrange_hub_name("A-a，B-b"), "Aa/Bb")
  # 並べ替えるので入力の順序には依らない
  expect_equal(arrange_hub_name("A-b，A-a"), "Aa/b")
  # 重複は落とす
  expect_equal(arrange_hub_name("A-a，A-a"), "Aa")
})

test_that("arrange_hub_name() の区切りは全角カンマ・全角セミコロン・スラッシュ", {
  expect_equal(arrange_hub_name("A-a；A-b"), "Aa/b")
  expect_equal(arrange_hub_name("A-a/A-b"),      "Aa/b")
})

test_that("arrange_hub_name() は lato_stricto の無い和名から NA を落とす", {
  expect_equal(arrange_hub_name("A，B"), "A/B")
  expect_equal(arrange_hub_name("A"), "A")
})

test_that("arrange_hub_name() は文字列以外に空文字を返す", {
  expect_equal(arrange_hub_name(NA), "")
  expect_equal(arrange_hub_name(123), "")
  expect_equal(arrange_hub_name(NULL), "")
})

test_that("arrange_hub_name() はベクトルを受け取れる", {
  expect_equal(arrange_hub_name(c("A-a，A-b", "C")), c("Aa/b", "C"))
})

test_that("arrange_hub_name() は広義・狭義の両方を 1 つにまとめる", {
  # 「ワメイ-広義，ワメイ-狭義」．順序はロケールに依るので中身だけを見る
  x <- stringi::stri_unescape_unicode(
    "\u30ef\u30e1\u30a4-\u5e83\u7fa9\uff0c\u30ef\u30e1\u30a4-\u72ed\u7fa9")
  res <- arrange_hub_name(x)
  expect_length(res, 1L)
  # 和名は 1 度だけ，広義と狭義は「/」で併記される
  expect_equal(lengths(strsplit(res, "/")), 2L)
  expect_true(grepl(stringi::stri_unescape_unicode("\u5e83\u7fa9"), res))
  expect_true(grepl(stringi::stri_unescape_unicode("\u72ed\u7fa9"), res))
  expect_equal(lengths(regmatches(res, gregexpr(
    stringi::stri_unescape_unicode("\u30ef\u30e1\u30a4"), res))), 1L)
})

test_that("hub2plus() と arrange_hub_name() は hub_master でつながる", {
  x <- hub_master %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_remove_all(., "[()]")) %>%
    dplyr::slice(1:50)
  hub_plus <- hub2plus(x$Hub_name, x$lato_stricto)
  expect_length(hub_plus, 50L)
  expect_false(any(grepl("NA", hub_plus)))
  res <- arrange_hub_name(hub_plus)
  expect_length(res, 50L)
  expect_type(res, "character")
})

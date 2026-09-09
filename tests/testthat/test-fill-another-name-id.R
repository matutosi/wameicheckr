# fill_another_name_id() の特性テスト．
# jn_master の another_name_ID が空欄のとき，ID (種) ごとに 0 から連番を振る．

test_that("fill_another_name_id() は空欄に ID ごとの連番を振る", {
  x <- tibble::tibble(ID = c("1", "1", "1", "2", "2"),
                      another_name_ID = rep(NA_character_, 5))
  res <- fill_another_name_id(x)
  expect_equal(res$another_name_ID, c(0, 1, 2, 0, 1))
  # 数値になって返る
  expect_type(res$another_name_ID, "double")
})

test_that("fill_another_name_id() は入力の行の順序を保つ", {
  x <- tibble::tibble(ID = c("1", "2", "1", "2"),
                      another_name_ID = rep(NA_character_, 4))
  res <- fill_another_name_id(x)
  expect_equal(res$ID, x$ID)
  # ID が隣と違えば 0 に戻る(並べ替えはしない)
  expect_equal(res$another_name_ID, c(0, 0, 0, 0))
})

test_that("fill_another_name_id() は作業用の列を残さない", {
  x <- tibble::tibble(ID = c("1", "1"), another_name_ID = c(NA_character_, NA))
  res <- fill_another_name_id(x)
  expect_named(res, c("ID", "another_name_ID"))
  expect_s3_class(res, "tbl_df")
})

test_that("fill_another_name_id() は既にある ID をそのまま残す", {
  # 空欄と値のあるものが混ざる場合．連番は空欄の側だけで振り直すので，
  # 既にある 0 と重なることがある(現状の振る舞い．仕様の追認ではない)．
  x <- tibble::tibble(ID = c("1", "1", "2", "2"),
                      another_name_ID = c("0", NA, NA, NA))
  res <- fill_another_name_id(x)
  expect_equal(res$another_name_ID, c(0, 0, 0, 1))
})

test_that("fill_another_name_id() は空欄が 1 つも無いと落ちる", {
  # purrr::accumulate() が空のベクトルを受け取るため．
  # 空欄のある jn_master 全体では起きないが，部分集合を渡すと出る．
  x <- tibble::tibble(ID = c("1", "1"), another_name_ID = c("0", "1"))
  expect_error(fill_another_name_id(x), "init")
})

test_that("fill_another_name_id() は jn_master を通せる", {
  jn <- jn_master %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[ /]", "_")) %>%
    dplyr::rename_with(~stringr::str_remove_all(., "[()]"))
  expect_true(any(is.na(jn$another_name_ID)))
  res <- fill_another_name_id(jn)
  expect_equal(nrow(res), nrow(jn))
  expect_named(res, names(jn))
  # 空欄はすべて埋まる
  expect_false(any(is.na(res$another_name_ID)))
  # 元から値のある行は変わらない
  ok <- ! is.na(jn$another_name_ID)
  expect_equal(res$another_name_ID[ok], as.numeric(jn$another_name_ID[ok]))
})

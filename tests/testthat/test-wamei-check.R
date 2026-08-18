# 特性テスト．いまの出力をそのまま固定して，分割・共通化で振る舞いが
# 変わっていないことを確かめる．仕様の追認ではないので，出力が変わったら
# 「意図した変更か」を必ず確認してから snapshot_accept() すること．

test_that("wamei_check() の分岐が想定どおりに動く", {
  x <- wamei_test_input()
  res <- wamei_check(x, hub_master, jn_master)
  expect_equal(nrow(res), length(x))
  expect_equal(res$input, x)
  expect_equal(res$n_match, c(1, 1, 2, 3, 0))
})

test_that("wamei_check() の出力が変わっていない", {
  x <- wamei_test_input()
  expect_snapshot_value(wamei_check(x, hub_master, jn_master, wide = TRUE),
                        style = "serialize")
  expect_snapshot_value(wamei_check(x, hub_master, jn_master, wide = FALSE),
                        style = "serialize")
})

test_that("wamei_check() は ds でデータソースを絞れる", {
  x <- wamei_test_input()
  expect_snapshot_value(wamei_check(x, hub_master, jn_master, ds = c(GL, SF)),
                        style = "serialize")
  expect_snapshot_value(wamei_check(x, hub_master, jn_master, ds = c(GL, SF, WF)),
                        style = "serialize")
})

test_that("wamei_check_ex() の出力が変わっていない", {
  x <- wamei_test_input()
  res <- wamei_check_ex(x, hub_master, jn_master)
  expect_equal(nrow(res), length(x))
  expect_equal(res$input, x)
  expect_snapshot_value(res, style = "serialize")
  expect_snapshot_value(wamei_check_ex(x, hub_master, jn_master, wide = FALSE),
                        style = "serialize")
})

test_that("wamei_check() は 1 件だけ渡しても動く", {
  x <- wamei_test_input()[1]
  expect_equal(nrow(wamei_check(x, hub_master, jn_master)), 1L)
  expect_equal(nrow(wamei_check_ex(x, hub_master, jn_master)), 1L)
})

test_that("wamei_check() は該当なしだけでも動く", {
  x <- wamei_test_input()[5]
  res <- wamei_check(x, hub_master, jn_master)
  expect_equal(nrow(res), 1L)
  expect_equal(res$n_match, 0)
})

# 特性テスト(characterization test)用の固定入力．
#
# wamei_check() と wamei_check_ex() を分割・共通化していくあいだ，
# 振る舞いが変わっていないことを確かめるためのもの．
# hub_master の該当件数で分岐が変わるので，1 件・2 件・3 件・0 件を並べる．
# 入力は hub_master から選んだ実在の和名(件数は 2026-08-18 時点のデータ)．
wamei_test_input <- function() {
  c(
    "\u30a2\u30fc\u30c6\u30a3\u30c1\u30e7\u30fc\u30af",                     # アーティチョーク  1 件
    "\u30a2\u30e0\u30fc\u30eb\u30a4\u30ef\u30ce\u30ac\u30ea\u30e4\u30b9",   # アムールイワノガリヤス  1 件
    "\u30a2\u30a4\u30c0\u30ac\u30e4",                                       # アイダガヤ  2 件
    "\u30a2\u30aa\u30b9\u30ba\u30e9\u30f3",                                 # アオスズラン  3 件
    "\u30ca\u30a4\u30e8"                                                    # ナイヨ  0 件(該当なし)
  )
}

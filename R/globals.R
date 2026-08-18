# R CMD check の "no visible binding for global variable" を止めるための宣言．
#
# 中身はすべて dplyr などに渡している「列名」と，遅延読み込みのデータセット
# (`ref_jp` `ref_sc`)．関数でも変数でもないので，実行時に問題は起きない．
#
# これは暫定対応．TODO.txt の「wamei_check(), wamei_check_ex() の
# 非標準評価(NSE)のコードを修正」を済ませれば，列名の分は不要になる．
# 新しいコードでは列名を裸で書かず，`.data[["col"]]` や文字列で渡すこと．
utils::globalVariables(c(
  # 遅延読み込みのデータセット
  "ref_jp", "ref_sc",
  # magrittr のプレースホルダ
  ".",
  # 和名チェックリストの列名
  "GL", "Hub_name", "ID", "ID2", "SF", "WF", "YL",
  "all_name", "another_name", "another_name_ID", "common_name",
  "hub_plus", "lato_stricto", "name_jp", "name_sc", "row_num",
  "scientific_name_with_author", "scientific_name_without_author",
  "st", "status",
  # 処理の途中で作る列名
  "dist", "dist_norm", "input", "n_match", "tmp"
))

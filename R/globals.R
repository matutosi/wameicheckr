# R CMD check の "no visible binding for global variable" を止めるための宣言．
#
# 列名は .data[["col"]] や文字列で渡すように直したので，ここに残るのは
# 列名ではないものだけ．
utils::globalVariables(c(
  # 遅延読み込みのデータセット．LazyData: true で名前空間から見えるが，
  # コード検査は関数の外にある変数として扱う．
  "ref_jp", "ref_sc",
  # ds のデータソース名．ds = c(GL, SF, WF, YL) は意図した tidy-eval で，
  # 利用者も vignette のように ds = c(GL, SF, WF) と書ける(0.9.3 で維持を決定)．
  "GL", "SF", "WF", "YL",
  # 廃止予定の search_similar_name() だけで使っている．
  # 0.10.0 で同関数を削除するときに，ここも消す．
  ".", "dist", "dist_norm", "input", "maybe", "tmp"
))

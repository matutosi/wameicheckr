# wameicheckr

維管束植物和名チェックリスト(和名チェックリスト)を使って，和名・学名を照合する R パッケージ．

- リポジトリ: https://github.com/matutosi/wameicheckr
- ブランチ: `main` のみ(分岐せず main で作業する)
- 現行バージョン: DESCRIPTION 0.9.3

## 構成

- `R/`: パッケージ本体の R コード
  - `wamei_check.R` `wamei_check_ex.R`: 和名チェックの主関数
  - `editdist_multi.R`: 編集距離(複数対応)と標準化編集距離
  - `arrange_hub_name.R` `fill_another_name_id.R` `hub2plus.R` `prep_data.R`: 下請け
  - `maybe.R`: `maybe()` `mosiya()`．類似した学名・和名の検索
  - `search_similar_name.R`: 0.9.3 で非推奨．0.10.0 で削除(下記「これからの作業」)
  - `wamei_check_parts.R`: `wamei_check()` と `wamei_check_ex()` の共通の段階
  - `clean_colnames.R`: チェックリストの列名の正規化
  - `globals.R`: `utils::globalVariables()`．列名以外だけが残っている
  - `RcppExports.R`: 自動生成．手で編集しない
- `src/`: Rcpp の C++ コード
  - `editdist.cpp`: `str2strvec()` `editdist()` (DP 版)
  - `RcppExports.cpp`: 自動生成．手で編集しない
- `tools/`: パッケージに含めない実験・下書き置き場(`.Rbuildignore` 対象外だが `R CMD build` には入らない配置)
- `tests/testthat/`: テスト．`test-wamei-check.R` は特性テストで，
  スナップショットは CRAN では skip される．`NOT_CRAN=true` で走らせる
- `data/` `man/` `vignettes/` `inst/`: データ・ドキュメント
- `archive/` `zip/`: 過去のビルド成果物

## 注意点

- `R/RcppExports.R` と `src/RcppExports.cpp` は `Rcpp::compileAttributes()` の生成物．
  直接編集せず，`src/*.cpp` の `// [[Rcpp::export]]` を直してから再生成する．
- `src/` に `int main()` を置かない．R のパッケージは共有ライブラリなので
  `main()` は不要で，`R CMD check` でも問題になる．ベンチマークや動作確認の
  `main()` を持つコードは `tools/` に置く．
- `src/*.o` `*.dll` は `src/.gitignore` で除外済み．
- **x280-home (`LAPTOP-ONKK9573`) では，そのままでは C++ がリンクできない**
  (2026-09-10)．Rtools が入っておらず，`sh` と `g++` が w64devkit
  (`D:/pf/w64devkit/bin`)のものになる．R の `SHLIB_CXXLD` は
  `g++ -std=gnu++20` の 2 語で，リンクの段で
  `sh: g++ -std=gnu++20  : not found` と落ちる(コンパイルは通る)．
  **リポジトリは触らず，環境変数で回避する**．
  `SHLIB_CXXLD = g++` の 1 行だけを書いた一時ファイルを作り，
  `R_MAKEVARS_USER` にその絶対パスを入れてから `R CMD INSTALL` などを回す．
  これが無いと `R CMD INSTALL`・`devtools`・`pkgload::load_all()` がすべて落ちる．
  なお PowerShell では `R` が `Invoke-History` の別名なので，`R.exe` と書く．
- **`R CMD check` は tar ball に対して行う**．ソースディレクトリを直接指定すると
  `Required fields missing or empty: 'Author' 'Maintainer'` で落ちる．
  `Authors@R` から展開されるのは `R CMD build` のときのため．
  vignette 込みで測るので `--no-build-vignettes` は付けない．

## これからの作業

2026-08-18 に検討．**1 から 5 の順で進める**(1 と 3 は 2026-09-10 に完了)．
根拠の実測値は下の「進捗状況」の測定結果も見ること．

### ~~1. テストを足す(2 と 4 の前提)~~ → **済んだ**(2026-09-10)

`arrange_hub_name()` `hub2plus()` `fill_another_name_id()`
`maybe()` `mosiya()` `str2strvec()` の 6 つに，テストが 1 件も無かったのを埋めた
(新規 65 件．`test-arrange-hub-name.R` `test-fill-another-name-id.R`
`test-maybe.R` と，`test-editdist.R` への追記)．

**分かったこと 2 つ**(どちらも直さず，現状をテストに固定した)．

- **`fill_another_name_id()` は空欄が 1 つも無いと落ちる**．
  `purrr::accumulate()` が空のベクトルを受け取るため．
  `jn_master` 全体(53,222 行中 5,289 行が空欄)では起きないが，
  **部分集合を渡すと出る**．
- **`maybe()` `mosiya()` の `left_join` が many-to-many の警告を出す**．
  `wamei_check()` は 0.9.3 で `relationship` を明示したが，この 2 つは未対応．
  **2 で 2 つを 1 つにまとめるときに併せて直す**．

書くときの注意．並べ替えに `sort()` を使う `arrange_hub_name()` は，
日本語の順序がロケールに依るので，順序を確かめる例は ASCII だけにした．

### 2. maybe() / mosiya() の絞り込みを C++ へ移し，2 つを 1 つにまとめる

**速度とメモリ**．実測は下の「maybe() / mosiya() の測定」節．
返す 395 行のために 155 万行の tibble を作っている(0.03 %)．

`src/editdist_bp.cpp` の `editdist_pairs()` の隣に，**`min_dist` 未満または
`min_dist_norm` 未満のペアだけを返す**内部関数を足す．返すのは
(入力の添字, 参照の添字, `editdist`, `editdist_norm`) の 4 列でよい．
`maybe()` `mosiya()` はそれを使う．約 3 倍速くなり，メモリはほぼ不要になる．

`editdist_multi()` は「全組み合わせを返す」ことが公開仕様なので変えない．

**2 つを 1 つにまとめる**．いまは実質 5 行しか違わない．

| | `maybe()` | `mosiya()` |
|---|---|---|
| `len` | 1 | 6 |
| `min_dist` | 4 | 3 |
| 参照 | `ref_sc$name_sc` | `ref_jp$name_jp` |
| 結合キー | `name_sc` | `name_jp` |
| 参照側の前処理 | 無し | `stri_unescape_unicode()` |

方針(2026-08-18 に決定)．

- `maybe()` は英語話者用，`mosiya()` は日本語話者用の名前とする．
- **どちらかを本体にして，もう一方はラッパーにする**．
- 本体は `len` で参照を選ぶ(`len == 6` なら `ref_jp`，それ以外は `ref_sc`)．
  `search_similar_name()` が元々そうしていた．
- `R/maybe.R` の末尾にコメントアウトで残る旧 `mosiya()` が，まさに `maybe()` の
  ラッパーだった．この形に戻すことになる．ただし旧版は `inp_esc` を渡して
  いないので，そのまま復活させない．

ついでに直すもの．`maybe()` の `inp_esc = TRUE` は無意味．`editdist_multi()` は
この引数を `len == 6` のときしか見ないので，`len = 1` の `maybe()` では無視される．

### ~~3. usethis と readxl を Suggests へ~~ → **済んだ**(2026-09-10)

`Imports` から `Suggests` へ移した．無い環境で `prep_*()` を呼んだときは，
`R/prep_data.R` の末尾に足した `stop_if_not_installed()`(`@noRd`)が
`install.packages()` を案内して止める．
vignette の `readxl` を使う塊は `eval = FALSE` なので影響しない．

### 4. superseded になった呼び出しの置き換え

| 箇所 | 現状 | 置き換え先 |
|---|---|---|
| `R/wamei_check.R:181` | `mutate_at(vars(contains(...)), ...)` | `across()` |
| `R/wamei_check.R:196-197` | `mutate_if(is.character, ...)` x2 | `across(where(is.character))` |
| `R/editdist_multi.R:49` | `mutate_at(c("s1","s2"), ...)` | `across(all_of(...))` |
| `R/arrange_hub_name.R:32` | `tidyr::separate()` | `separate_wider_delim()` |
| `R/maybe.R` x2, `R/search_similar_name.R` x1 | `magrittr::set_colnames()` | `rlang::set_names()` か `names()<-` |

いま壊れているわけではないが，`mutate_at()` `mutate_if()` `vars()` は superseded．
1 を済ませてから着手する．2 で書き換わる行を二度触らないよう，順序は最後．

### 5. 細かいもの

- `R/hub2plus.R:24`：`purrr::map2()` の返す list を `str_remove_all()` に
  渡していて，暗黙の文字列化に頼っている．`purrr::map2_chr()` にする．
- `R/arrange_hub_name.R:39`：`` `names<-`(NULL) `` は `unname()` で足りる．
- `.Rbuildignore:4`：`^README.\.Rmd$` は `.` が未エスケープで，次行の
  `^README.*\.R*md$` と重複している．
- `R/data.R`：`@format A data frame with 30430 rows` が手書きの数値．
  データを更新したときにずれる．
- `src/editdist.cpp`：`str2strvec()` の `int n = str.size()` は符号違い．
  `reserve()` も無い(`editdist_bp.cpp` 側には入れた)．

### 0.10.0 で

`search_similar_name()` を削除する．2026-08-18 に非推奨にした(0.9.3)．
削除は `R/search_similar_name.R` を消し，`R/globals.R` の
`. dist dist_norm input maybe tmp` も消す．

## check の生成物の後始末

- **`R CMD check` などで作られる `*.tar.gz` は，役割が終わったら削除する**．
  結果を確認し終えたら (CRAN へ出す場合は提出が済んだら) 消してよい．
  DESCRIPTION とソースから何度でも作り直せるため，残しておく理由がない．
- 同じ理由で，`*.Rcheck/` (check の作業ディレクトリ) も確認が済んだら消す．
- 補足: `*.tar.gz` を作るのは `R CMD build` / `devtools::build()` で，
  `devtools::check()` は既定で一時ディレクトリに作るためプロジェクト直下には残らない．
  プロジェクト直下に残るのは `R CMD build` を直接実行したときが多い．
  どちらの経路でできたものでも，見つけたら消す．

## 進捗状況

### 現在の状態

- 2026-09-10 07:08 (このセッション，x280-home)
  「これからの作業」の **1(テストを足す)と 3(usethis・readxl を Suggests へ)を実施**．
  新規 65 件を含めテストは全通過，`R CMD check`(tar ball，vignette 込み)は **Status: OK**．
  次は **2. `maybe()` / `mosiya()` の C++ 化と統合**(many-to-many の警告も併せて直す)．

- 2026-08-19 02:31 更新
  旧 `TODO.txt` の課題(0〜5)を順に実施し，**すべて完了**(バージョン 0.9.3)．
  テストは 1,534 件すべて通過．`R CMD check`(tar ball)は **Status: OK**．
- それ以前は [notes/history.md](notes/history.md) を見る(高速化の実測値・
  R CMD check の手当て・分割で分かったこと・コミット履歴も同じファイル)．

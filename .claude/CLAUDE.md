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
- **`R CMD check` は tar ball に対して行う**．ソースディレクトリを直接指定すると
  `Required fields missing or empty: 'Author' 'Maintainer'` で落ちる．
  `Authors@R` から展開されるのは `R CMD build` のときのため．
  vignette 込みで測るので `--no-build-vignettes` は付けない．

## これからの作業

2026-08-18 に検討．実行は後日．**1 から 5 の順で進める**．
根拠の実測値は下の「進捗状況」の測定結果も見ること．

### 1. テストを足す(2 と 4 の前提)

テストが無い export 関数．

- `arrange_hub_name()` `hub2plus()` `fill_another_name_id()`
- `maybe()` `mosiya()` `str2strvec()`

テストがあるのは `editdist()` `editdist_multi()` `editdist_norm()`
`wamei_check()` `wamei_check_ex()` `search_similar_name()`．

`wamei_check()` のときと同じで，「振る舞いを変えない」変更をする前に，
変わっていないことを確かめる手段を作っておく．

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

### 3. usethis と readxl を Suggests へ

どちらも `R/prep_data.R` でしか使っていない．`prep_data_all()` `prep_hub_data()`
`prep_jn_data()` `prep_ref_data()` `read_hub_jn()` はすべて未 export で，
維持者が `data/` を作り直すときにしか動かない．

それなのに `Imports` にあるため，**全利用者が `usethis`(開発ツール)と `readxl` の
導入を強制される**．`Suggests` に移し，`requireNamespace()` で案内を出す．

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

2026-08-19 02:31 更新．

- 旧 `TODO.txt` の課題を順に実施し，**すべて完了**(バージョン 0.9.3)．
  0. 特性テストを先に用意．`wamei_check()` `wamei_check_ex()` にはテストが
     無く，分割の前後で出力が変わらないことを確かめる手段が無かった．
  1. `search_similar_name()` を非推奨に(`.Deprecated()`)．削除は 0.10.0．
     `maybe()` `mosiya()` は `R/maybe.R` へ移した．
  2. 列名の正規化を `clean_colnames()` に切り出し(6 箇所の重複)．
  3. 2 つの関数を同じ段階名の内部関数に分割．
  4. 本当に一致する 5 段階を `R/wamei_check_parts.R` にまとめた．
  5. NSE を `.data[["col"]]` と文字列に修正．`R/globals.R` は 27 → 12 項目．
- テストは 1,534 件すべて通過．`R CMD check`(tar ball)は **Status: OK**．
- `ds = c(GL, SF, WF, YL)` は意図した tidy-eval なので**維持**(利用者の判断)．
  そのため `GL` `SF` `WF` `YL` は `R/globals.R` に残る．
- 分割の途中で見つかった既存バグも修正(利用者の判断)．
  - `read_hub_jn()` の余分な `%>%`．最後の代入が `list()` へパイプされていた．
    ツルボラン → ワスレグサ の置換が効かず，戻り値も 3 要素の list だった．
  - 同じブロックで `stri_unescape_unicode()` が 1 箇所抜けており，
    シベリアカラマツ(キンポウゲ科)の判定が絶対に成立しなかった．
  - `data/ref_jp.rda` `data/ref_sc.rda` を作り直した．**中身は同一**．
    この 2 つは ID・和名・学名から作り，科名を使わないため．
  - `wc_multi_match()` の `id` との join に `relationship = "many-to-many"`
    を明示．2 件以上該当する和名は status もデータソースも複数あるので，
    多対多が正しい．
- 旧 `TODO.txt`(git 管理外)を廃止し，内容をこのファイルの
  「これからの作業」へ統合した．課題の置き場はここ 1 つにする．
  `.gitignore` と `.Rbuildignore` の `TODO.txt` の行も消した．
- `TODO.txt` 廃止に伴う設定(`.Rbuildignore` `.gitignore`)と，記録の更新
  (`NEWS.md` `.claude/CLAUDE.md`)を別々のコミットに分けた．
  R のコードは触っていないので，テスト 1,534 件通過・
  `R CMD check` **Status: OK** は上のまま有効．
- 次は「これからの作業」の **1. テストを足す**から始める(2 と 4 の前提)．

### 詳しくはこちら

高速化の実測値・R CMD check の手当て・分割で分かったこと・コミット履歴
(2026-08-18 時点)は [.claude/notes/history.md](notes/history.md) にある．

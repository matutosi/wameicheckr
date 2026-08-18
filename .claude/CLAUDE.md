# wameicheckr

維管束植物和名チェックリスト(和名チェックリスト)を使って，和名・学名を照合する R パッケージ．

- リポジトリ: https://github.com/matutosi/wameicheckr
- ブランチ: `main` のみ(分岐せず main で作業する)
- 現行バージョン: DESCRIPTION 0.9.2

## 構成

- `R/`: パッケージ本体の R コード
  - `wamei_check.R` `wamei_check_ex.R`: 和名チェックの主関数
  - `editdist_multi.R`: 編集距離(複数対応)と標準化編集距離
  - `arrange_hub_name.R` `fill_another_name_id.R` `hub2plus.R` `prep_data.R`: 下請け
  - `search_similar_name.R`: 廃止予定(TODO.txt 参照)
  - `RcppExports.R`: 自動生成．手で編集しない
- `src/`: Rcpp の C++ コード
  - `editdist.cpp`: `str2strvec()` `editdist()` (DP 版)
  - `RcppExports.cpp`: 自動生成．手で編集しない
- `tools/`: パッケージに含めない実験・下書き置き場(`.Rbuildignore` 対象外だが `R CMD build` には入らない配置)
- `data/` `man/` `vignettes/` `inst/`: データ・ドキュメント
- `archive/` `zip/`: 過去のビルド成果物
- `TODO.txt`: 課題メモ(git 管理外)

## 注意点

- `R/RcppExports.R` と `src/RcppExports.cpp` は `Rcpp::compileAttributes()` の生成物．
  直接編集せず，`src/*.cpp` の `// [[Rcpp::export]]` を直してから再生成する．
- `src/` に `int main()` を置かない．R のパッケージは共有ライブラリなので
  `main()` は不要で，`R CMD check` でも問題になる．ベンチマークや動作確認の
  `main()` を持つコードは `tools/` に置く．
- `src/*.o` `*.dll` は `src/.gitignore` で除外済み．

## 進捗状況

### 現在の状態

2026-08-18 10:43 更新．

- `editdist_multi()` の高速化(済)．`src/editdist_bp.cpp` の `editdist_pairs()` が
  全組み合わせを C++ 内で一括計算する．`editdist_bp()` は Myers の bit-parallel 法．
  どちらも非 export の内部関数(`//' @noRd`)．
- 積み残しだった 3 件を実施(済)．
  - `bp_min` の既定値を実データで測り直し，16 → **18**．
  - `editdist()` の可変長配列を，表を 2 行だけ持つ実装に差し替え．
    共通部分は `src/editdist.h` 経由で `editdist_bp.cpp` と共有．
  - `roxygen2` 8.1.0 で `roxygenise()` を実行．差分は 3 ファイル 7 行だけだった．
    `RoxygenNote` は `Config/roxygen2/version` に置き換わる．
- `R CMD check`(tar ball)で出た指摘のうち，次を直した．
  - `.claude` が tar ball に入る → `.Rbuildignore` に `^\.claude$` を追加．
  - `editdist_multi()` の `s1` `s2` が no visible binding → `mutate()` をやめて
    列を直接作る形に変更．
  - `editdist_multi.Rd` の未記載引数(`inp_esc` `ref_esc` `editdist`)→ `@param` を追加．
  - `Depends: R (>= 3.5.0)` だが `R/editdist_multi.R` がネイティブパイプ `|>` を
    使っている(コミット `3276159` から) → `R (>= 4.1.0)` に変更．
- テストは 1,517 件すべて通過．`editdist()` 自体は `utils::adist()` を正解として
  独立に検証している(高速版のテストは `editdist()` を正解とするため)．
- `R CMD check`(tar ball, `--no-manual`)は **Status: OK**．
  着手前は 4 WARNINGs, 3 NOTEs だった．解消の内訳は下記．
- 未着手の課題は `TODO.txt` にある
  (`wamei_check()` `wamei_check_ex()` の分割・NSE 修正，`search_similar_name()` の廃止)．

### 測定結果(2026-08-18)

高速化の判断に使った実測値．同じ判断を繰り返さないために残す．

- **ボトルネックは編集距離の計算ではなく，ペアごとに R から `.Call` する部分だった**．
  `editdist_multi()` 50 x 500 で 0.55s → 0.03s(18 倍)，100 x 2,000 で 1.47s → 0.25s(6 倍)．
  規模が大きいほど `expand_grid()` の tibble 生成が支配的になるので，倍率は下がる．
- **bit-parallel 法の効果は限定的**．C++ 内で一括ループさせた純粋な計算時間の比較:

  | 文字列長 | DP | bit-parallel | 比 |
  |---|---|---|---|
  | 5-15 文字 | 0.08s | 0.13s | 1.6 倍 遅い |
  | 20-40 文字 | 0.48s | 0.36s | 0.75 |
  | 60-100 文字 | 3.67s | 2.48s | 0.68 |
  | 150-250 文字 | 17.67s | 17.85s | 1.0 (fallback) |

  短い文字列で遅いのは，ペアごとに `unordered_map<string, uint64_t>` を作る
  コストが DP 本体より重いため．和名は 3-13 文字なので DP のままが速い．
- **`bp_min` の掃引**．まず乱数文字列(100 x 5,000)で 12-24 が横ばいと分かり，
  次に実データで測り直した．入力 30 件 x 参照全件:

  | `bp_min` | 0(常に DP) | 8 | 10 | 12 | 14 | 16 | **18** | 20 | 24 | 32 |
  |---|---|---|---|---|---|---|---|---|---|---|
  | 学名 `ref_sc` 76,379 件 | 6.71s | 4.65 | 4.64 | 4.62 | 4.57 | 4.58 | **4.52** | 4.52 | 4.79 | 5.45 |
  | 和名 `ref_jp` 51,809 件 | 0.59s | 0.61 | 0.59 | 0.59 | 0.59 | 0.59 | **0.59** | 0.59 | 0.59 | 0.59 |

  学名は 8-20 がほぼ横ばいで 18-20 が最速(常に DP の 0.67 倍)．24 以上は
  DP に落ちる分だけ遅くなる．和名は 99.9 % が 16 文字未満なので，どの値でも変わらない
  (中央値 7 文字，最長 25 文字．学名は中央値 28 文字で 94.5 % が 16 文字以上)．
  **既定値は 18**(`src/editdist_bp.cpp` の 2 つの `bp_min = 18`)．

- bit-parallel の元コード(`tools/editdist2.cpp`)の `1L << 63` は Windows では
  `long` が 32 bit のため壊れる．`uint64_t(1) << 63` にすること．
  多ブロック版は使わず，64 トークンを超えたら DP に落としている．

### 積み残し

- `TODO.txt` の課題(`wamei_check()` `wamei_check_ex()` の分割・NSE 修正，
  `search_similar_name()` の廃止)．
  NSE はいま `R/globals.R` の `utils::globalVariables()` で NOTE を
  止めているだけの暫定対応．済ませれば列名の分は不要になる．
- `R CMD check` はソースディレクトリを直接指定すると
  `Required fields missing or empty: 'Author' 'Maintainer'` で落ちる．
  `Authors@R` から展開されるのは `R CMD build` のときなので，
  **tar ball を作ってから check する**こと(元からの挙動で，今回の変更とは無関係)．

### R CMD check を Status: OK にした手当て(2026-08-18)

- **非 ASCII**．問題になるのは**文字列リテラルだけ**で，コメントと roxygen は
  日本語のままでよい(実測で確認)．`getParseData()` の `STR_CONST` を見れば
  対象を機械的に洗い出せる．`stringi::stri_escape_unicode()` で変換した．
- **`vignettes/` はあるが `inst/doc/` が無い WARNING は，`--no-build-vignettes`
  の副作用**．普通に `R CMD build` すれば `inst/doc/` が作られて消える．
  `.gitignore` が `inst/doc` を除いているのは正常．
- **`data(hub_master)` などは削除**．`LazyData: true` なので不要で，
  `data()` は既定で `.GlobalEnv` へ読み込むため，呼ぶと利用者の環境を汚す．
- `Imports` の `knitr` `rmarkdown` `tidyverse` は `Suggests` へ．
  `tidyverse` は vignette と例でしか使っておらず，例からは削除した．
- **check はソースディレクトリではなく tar ball に対して行う**
  (`Authors@R` の展開が `R CMD build` のときのため)．
  vignette も込みで測るので `--no-build-vignettes` は付けない．

### コミット履歴

- `036a175` declare the column names used by NSE (2026-08-18)
- `6052d2b` move knitr, rmarkdown and tidyverse to Suggests (2026-08-18)
- `a591799` stop calling data() and qualify tidyselect (2026-08-18)
- `02a648c` escape non-ASCII string literals (2026-08-18)
- `a3dc898` verify editdist() against utils::adist() (2026-08-18)
- `38575d6` regenerate documentation with roxygen2 8.1.0 (2026-08-18)
- `35e72d6` fix what R CMD check reported (2026-08-18)
- `cb34da2` drop the variable length array in editdist() (2026-08-18)
- `2ef42e5` record the speed-up in NEWS.md and .claude/CLAUDE.md (2026-08-18)
- `23b0313` add tests for edit distance (2026-08-18)
- `e95d91a` compute edit distance of all pairs in C++ (2026-08-18)
- `8b4ec47` add .claude/CLAUDE.md (2026-08-18)
- `99a140d` move editdist2.cpp from src/ to tools/ (2026-08-18)
- `3276159` update documents (2024-08-10)

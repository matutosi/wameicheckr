# 経緯・測定記録

過去の判断根拠と作業ログ．日常の参照には要らないので `.claude/CLAUDE.md` から切り出した．

## 測定結果(2026-08-18)

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

## R CMD check を Status: OK にした手当て(2026-08-18)

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

## maybe() / mosiya() の測定(2026-08-18)

次の作業の根拠．

- `mosiya()` 30 件 x 51,809 = **1.91 s**，結果 395 行．
  `maybe()` 10 件 x 76,379 = **2.32 s**，結果 32 行．
- **返す 395 行のために 155 万行の tibble を作っている**(0.03 %)．メモリ 111 MB．
  距離計算そのものは 0.59 s なので，残り約 1.3 s は `expand_grid()` の
  tibble 生成と直後の `filter()`．絞り込みを C++ へ移せば約 3 倍になる．
- `editdist_multi()` は「全組み合わせを返す」のが公開仕様なので変えない．
  内部関数を別に足す．
- `maybe()` と `mosiya()` は実質 5 行しか違わない．
  **`maybe()` は英語話者用，`mosiya()` は日本語話者用の名前**とし，
  どちらかを本体，もう一方をラッパーにする(利用者の判断)．
  本体は `len` で参照を選ぶ(`len == 6` なら `ref_jp`)．
  `R/maybe.R` の末尾にコメントアウトで残る旧 `mosiya()` がその形だった．

## 分割で分かったこと(2026-08-18)

- **特性テストが先に要る**．`wamei_check()` は `data/` のデータだけで
  no_match / single / multi・wide / long・`ds` の全分岐に到達できる．
  `expect_snapshot_value(style = "serialize")` は CRAN では skip されるので，
  手元では `NOT_CRAN=true` を付けて走らせる．
- **NSE を明示にすると隠れたバグが出る**．2 件以上該当する和名が無いと
  `pivot_wider()` が `message` 列を作らず，裸の `message` が `base::message`
  に解決されていた．行数が 0 だったので誤りに気づけなかった．
- **`:=` は dplyr から export されていない**(rlang のもの)．列名を動的に
  付けるためだけに rlang を Imports へ足すのは避け，`transmute()` で仮の
  名前を付けてから `names()` で差し替えた(列順を変えないため)．
- **many-to-many の警告が出る join を特定するには，段階の関数を順に呼ぶ**．
  `wamei_check()` をそのまま呼んでも警告が表に出ないことがあり，
  `wc_multi_match()` の中の `id` との join だと分かるまで手間取った．
  `relationship` は本当に多対多の join だけに書く(全部に書くと検査が無効になる)．
- **`read_hub_jn()` に行末の余分な `%>%` があった**．代入文の右辺が
  `list()` へパイプされ，`x[i] <- value` の値がそのまま戻り値になるため，
  呼び出し側からは正常に見えていた．行末のパイプは目視で見つけにくい．

## コミット履歴

- `6c60c28` state that the id join is many-to-many (2026-08-18)
- `15ff346` fix the stray pipe in read_hub_jn() (2026-08-18)
- `7e80f30` record finishing the TODO list (2026-08-18)
- `ef8e8ad` say which names are columns (2026-08-18)
- `34c399c` merge the steps the two functions share (2026-08-18)
- `4a08ae0` split wamei_check() and wamei_check_ex() into named steps (2026-08-18)
- `02bbd92` extract clean_colnames() (2026-08-18)
- `d8e0dcc` deprecate search_similar_name() (2026-08-18)
- `aea160e` pin down what wamei_check() returns today (2026-08-18)
- `5690c2d` record that R CMD check is now clean (2026-08-18)
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

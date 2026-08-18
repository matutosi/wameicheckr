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

2026-08-18 10:15 更新．

- `editdist_multi()` の高速化を実施．
  - `src/editdist_bp.cpp` を追加．`editdist_pairs()` が全組み合わせを C++ 内で
    一括計算し，`editdist_bp()` が Myers の bit-parallel 法を提供する．
    どちらも非 export の内部関数(`//' @noRd`)．
  - `R/editdist_multi.R` が `purrr::pmap_int()` をやめて `editdist_pairs()` を呼ぶ．
    `editdist_norm()` は `max()` → `pmax()` でベクトル対応．
  - `tests/testthat/` を新設．高速版が `editdist()` と一致することを乱数で確認．
    1,117 件すべて通過．
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
- **`bp_min` の掃引**(100 x 5,000)．和名(len=6)は差が出ない．学名(len=1, 15-45 文字)のみ:

  | `bp_min` | 0(常に DP) | 8 | 12 | 16 | 20 | 24 | 32 | 64 |
  |---|---|---|---|---|---|---|---|---|
  | 時間 | 1.05s | 0.86 | 0.83 | 0.87 | 0.81 | 0.83 | 0.88 | 1.00 |

  12-24 の範囲はほぼ横ばい．既定は 16(`src/editdist_bp.cpp` の 2 つの
  `bp_min = 16`)．**要再検討**．
- bit-parallel の元コード(`tools/editdist2.cpp`)の `1L << 63` は Windows では
  `long` が 32 bit のため壊れる．`uint64_t(1) << 63` にすること．
  多ブロック版は使わず，64 トークンを超えたら DP に落としている．

### 積み残し

- `bp_min` の既定値 16 の再検討(上の掃引は乱数文字列．実データで測り直す)．
- `src/editdist.cpp` の `editdist()` は可変長配列 `int d[m+1][n+1]` を使っており，
  GCC 拡張かつ長い文字列でスタックを壊す恐れがある．
  `src/editdist_bp.cpp` の `editdist_dp()` は 2 行だけ持つ実装なので，
  `editdist()` の中身も差し替えられる．
- roxygen2 が 8.1.0，`RoxygenNote` は 7.3.2．`roxygenise()` を走らせると
  man 以下が全面的に書き換わるので，必要になるまで走らせていない．

### コミット履歴

- `8b4ec47` add .claude/CLAUDE.md (2026-08-18)
- `99a140d` move editdist2.cpp from src/ to tools/ (2026-08-18)
- `3276159` update documents (2024-08-10)
- `66bcf24` replace() -> remove()

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

2026-08-18 09:55 更新．

- 高速版の編集距離(bit-parallel 法)の C++ 実装を検討中．ステージ済みだった
  `src/editdist2.cpp` は，ベンチマーク用の `main()` を含んだまま
  `Rcpp::compileAttributes()` を実行しており，`main()` が R 関数として
  export される状態だった．
  - `tools/editdist2.cpp` へ移動し，`R/RcppExports.R` と `src/RcppExports.cpp`
    を HEAD の状態へ復元した．`Rcpp::compileAttributes()` 再実行で差分が
    出ないこと，`src/` が警告なくコンパイルできることを確認済み．
  - 次の検討: `main()`・`time_it()`・`test()` を削って
    Rcpp export 関数(`editdist()` の高速版)として `src/` へ組み込むか．
- 未着手の課題は `TODO.txt` にある
  (`wamei_check()` `wamei_check_ex()` の分割・NSE 修正，`search_similar_name()` の廃止)．
- テスト(`tests/testthat/`)が無い．高速版を入れるなら DP 版との一致テストが要る．

### コミット履歴

- `3276159` update documents (2024-08-10)
- `66bcf24` replace() -> remove()
- `04e3c31` do not calculate when same strings

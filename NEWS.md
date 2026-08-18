# 更新履歴

## wameicheckr (development version)

* `editdist_multi()`：ペアごとの計算を C++ 側へ移して高速化．
  和名 100 x 参照 2,000 で約 6 倍，50 x 500 で約 18 倍．
  返す内容は従来と同じ．

* `editdist_pairs()` (C++, 内部)：2 つの文字列ベクトルの全組み合わせの
  編集距離を C++ 内で一括計算．

* `editdist_bp()` (C++, 内部)：Myers の bit-parallel 法による編集距離．
  18 文字(`bp_min`)以上のときに使い，短いときは従来の動的計画法を使う．
  学名(`ref_sc`)の照合で約 3 割速い．和名は 99.9 % が 18 文字未満なので
  動的計画法のまま．

* `editdist()`：表を 2 行だけ持ち回る実装に変更．
  可変長配列 `int d[m+1][n+1]` をやめたので，長い文字列でもスタックを
  壊さない．返す値は従来と同じ．

* `editdist_norm()`：`max()` を `pmax()` に変更してベクトルに対応．
  1 組ずつ渡したときの結果は従来と同じ．

* `R CMD check` の指摘をすべて解消(**Status: OK**)．

  * 非 ASCII の文字列リテラル 15 か所を `\uxxxx` へ変更．
    `R/arrange_hub_name.R` `R/prep_data.R` `R/wamei_check.R`
    `R/wamei_check_ex.R`．コメントは日本語のまま．

  * `data(hub_master)` などの呼び出しを削除．`LazyData: true` なので不要で，
    `data()` は既定で `.GlobalEnv` へ読み込むため，利用者の環境を汚していた．

  * `all_of()` `any_of()` に `tidyselect::` を付与．

  * `knitr` `rmarkdown` `tidyverse` を `Imports` から `Suggests` へ移動．
    いずれもパッケージ本体では使っていない．

  * `R/globals.R`：列名による no visible binding を
    `utils::globalVariables()` で宣言．TODO.txt の NSE 修正までの暫定対応．

* `DESCRIPTION`：`Depends` を `R (>= 4.1.0)` に変更．
  `R/editdist_multi.R` がネイティブパイプ `|>` を使っているため．

* `tests/testthat/`：編集距離のテストを追加．
  `editdist()` が `utils::adist()` と一致すること，
  高速版が `editdist()` と一致することを乱数で確認する．


## wameicheckr 0.9.2

* updated at 20220307

* `editdist_multi()`：複数対応の編集距離の計算．

* `editdist_norm()`：標準化した編集距離の計算(個別)．editdist_multi()で使用．

* `mosiya()`, `maybe()`：`editdist_multi()`を使用するように変更．`search_similar_name()`は不要になった(はず)．

## wameicheckr 0.9.1

* `editdist()` (C++)：編集距離を算出可能．

* `str2strvec()` (C++)：stringをstring型のvectorに変換する．

* `search_similar_name()`：類似した和名・学名の検索(個別)．

* `mosiya()`：類似した和名の検索(複数)．

* `maybe()`：類似した学名の検索(複数)．


## wameicheckr 0.9.0

* `wamei_check()`, `wamei_check_ex()` ：維管束植物和名チェックリストでのデータを検索．

* `arrange_hub_name()` `fill_another_name_id()` `hub2plus()`：下請け関数．

* `hub_master` `jn_master`：和名チェックリスト作成したデータ．基本的には，元データと同じ．

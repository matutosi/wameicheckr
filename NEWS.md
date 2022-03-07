# 更新履歴

## wameicheckr (development version)


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

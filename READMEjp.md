
# wameicheckr

wameicheckrは，維管束植物の和名について和名チェックリストに
基づいて和名を検索するためのものです．和名チェックリストは，
以下からダウンロード可能です．

山ノ内 崇志・首藤 光太郎・大澤 剛士・米倉 浩司・加藤 将・志賀 隆. 2019.
「維管束植物和名チェックリスト」
(<https://www.gbif.jp/v2/activities/wamei_checklist.html>)

## Installation

wameicheckrは，[GitHub](https://github.com/)からインストールできます．

なお，主に対象とする使用者が日本語話者のみであるため，cranでの公開は
予定していません．

``` r
# install.packages("devtools")
devtools::install_github("matutosi/wameicheckr", build_vignettes = TRUE)
```

## Example

使用方法は，vignetteをご覧ください．

<https://github.com/matutosi/wameicheckr/blob/main/vignettes/wameicheckr.md>

``` r
vignette("wameicheckr")
```

## Citation

松村 俊和 (2021) Rを使った維管束植物和名チェックリストによる和名の確認.
<https://github.com/matutosi/wameicheckr/>.
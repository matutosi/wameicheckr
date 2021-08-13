# wameicheckr

The goal of wameicheckr is to check Japanese neme (wamei) of vascular
plant species by wamei checklist. You can download wamei checklist from
web page below. <https://www.gbif.jp/v2/activities/wamei_checklist.html>

Yamanouchi, T., Shutoh, K., Osawa, T., Yonekura, K., Kato, S., Shiga, T.
2019. A checklist of Japanese plant names
(<https://www.gbif.jp/v2/activities/wamei_checklist.html>)

wameicheckrは，維管束植物の和名について和名チェックリストに
基づいて和名を検索するためのものです．和名チェックリストは，
以下からダウンロード可能です．

山ノ内 崇志・首藤 光太郎・大澤 剛士・米倉 浩司・加藤 将・志賀 隆. 2019.
「維管束植物和名チェックリスト」
(<https://www.gbif.jp/v2/activities/wamei_checklist.html>)

## Installation

You can install the released version of wameicheckr from
[GitHub](https://github.com/) with:

wameicheckrは，[GitHub](https://github.com/)からインストールできます．

なお，主に対象とする使用者が日本語話者のみであるため，cranでの公開は
予定していません．

``` r
# install.packages("devtools")
devtools::install_github("matutosi/wameicheckr", build_vignettes = TRUE)
```

## Example

You can see examples from vignette.

使用方法は，vignetteをご覧ください．

<https://github.com/matutosi/wameicheckr/blob/main/vignettes/wameicheckr.md>

``` r
vignette("wameicheckr")
```

## Citation

Toshikazu Matsumura (2021) Check Japanese name (wamei) of vascular plant
species with R. <https://github.com/matutosi/wameicheckr/>.

松村 俊和 (2021) Rを使った維管束植物和名チェックリストによる和名の確認.
<https://github.com/matutosi/wameicheckr/>.

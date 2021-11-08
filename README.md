
Japanese README is
<https://github.com/matutosi/wameicheckr/blob/main/READMEjp.md> .

# wameicheckr

The goal of wameicheckr is to check Japanese neme (wamei) of vascular
plant species by wamei checklist. You can download wamei checklist from
web page below.

<https://www.gbif.jp/v2/activities/wamei_checklist.html>

Using reference data, We can find similar scientific names that does not
match exactly to existing names.

Yamanouchi, T., Shutoh, K., Osawa, T., Yonekura, K., Kato, S., Shiga, T.
2019. A checklist of Japanese plant names
(<https://www.gbif.jp/v2/activities/wamei_checklist.html>)

## Installation

You can install the released version of wameicheckr from \[GitHub\]
(<https://github.com/>). wameicheckr will not be released in cran,
because the main user is only Japanese speakers.

``` r
# install.packages("devtools")
# devtools::install_github("matutosi/wameicheckr", build_vignettes = TRUE) # needs pandoc
devtools::install_github("matutosi/wameicheckr")
```

You can install the binary package by downloading zip file (MS Windows).
<https://github.com/matutosi/wameicheckr/tree/main/zip>

## Example

You can see examples from vignette. Sorry Japanese only.

``` r
# vignette("wameicheckr") # please build vignettes in the installation
```

The page below is the same to vignette.

<https://github.com/matutosi/wameicheckr/blob/main/howtouse.md>

## Citation

Toshikazu Matsumura (2021) wameicheckr: Checker for Japanese name
(wamei) of vascular plant species with R.
<https://github.com/matutosi/wameicheckr/>.

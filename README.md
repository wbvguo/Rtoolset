
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rtoolset

<!-- badges: start -->

[![R-CMD-check](https://github.com/wbvguo/Rtoolset/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wbvguo/Rtoolset/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A tool set for R programming and data analysis

## Installation

You can install the development version of Rtoolset from
[GitHub](https://github.com/wbvguo/Rtoolset.git) with:

``` r
# install.packages("pak")
pak::pak("wbvguo/Rtoolset")
```

## Example

## create a dir when it does not exist

``` r
library(Rtoolset)
make_dir("~/test/")
#> [1] "~/test/ exists! Skip creating..."
```

<!-- ## Legacy -->
<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
<!-- You can also embed plots. In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->

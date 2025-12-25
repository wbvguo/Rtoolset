
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rtoolset <a href="https://github.com/wbvguo/Rtoolset/"><img src="man/figures/Rtoolset.png" align="right" height="138" alt="Rtoolset website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/wbvguo/Rtoolset/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wbvguo/Rtoolset/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A miscellaneous tool set for R programming and data analysis

## Installation

You can install the development version of Rtoolset from
[GitHub](https://github.com/wbvguo/Rtoolset.git) with:

``` r
# install.packages("pak")
pak::pak("wbvguo/Rtoolset")
```

## Usage

``` r
library(Rtoolset)
draw_xmas_tree_panel_gif()
```

<img src="man/figures/xmas_panel.gif" alt="" width="100%" />

<!-- check out the [vignette](https://htmlpreview.github.io/?https://github.com/wbvguo/Rtoolset/blob/main/vignettes/misc.html) page: -->

<!-- 1. [Rtoolset]() -->

<!-- 2. [RNAseq](https://wbvguo.github.io/Rtoolset/articles/RNAseq.html) -->

<!-- 3. [Methylation](https://wbvguo.github.io/Rtoolset/articles/Methylation.html) -->

<!-- ### folder & files -->

<!-- ####  create a dir when it does not exist -->

<!-- ```{r example} -->

<!-- library(Rtoolset) -->

<!-- mkdir("~/test/") -->

<!-- ``` -->

<!-- #### save to pdf -->

<!-- ```{r} -->

<!-- save2pdf(file = "./man/figures/save2pdf.pdf", width = 6, height = 6, overwrite = TRUE, -->

<!--          plot_code = quote({ -->

<!--            t = seq(0, 100, 1) -->

<!--            plot(cos(t) + t*sin(t), sin(t) - t* cos(t), type = "l", asp = 1) -->

<!--          })) -->

<!-- ``` -->

<!-- ### print -->

<!-- ```{r} -->

<!-- print_pval(0.0000123) -->

<!-- ``` -->

<!-- ```{r} -->

<!-- vec2print(c("apple", "banana")) -->

<!-- ``` -->

<!-- ### match -->

<!-- ```{r} -->

<!-- closestMatch("DFNB31", c("DNMT1", "DTNBP1", "IFNB1")) -->

<!-- ``` -->

### more

under construction…

<!-- ## Legacy -->

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->

<!-- ```{r cars} -->

<!-- summary(cars) -->

<!-- ``` -->

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->

<!-- You can also embed plots. In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->

# Install packages from CRAN, Bioconductor, or GitHub

Installs each entry in `pkgs` from the selected `repo`. Skips packages
that are already installed (unless `force = TRUE`). For GitHub, pass
`"owner/repo"` strings (e.g., `"rstudio/gt"`).

## Usage

``` r
install_packages(
  pkgs,
  repo = c("cran", "bioc", "github"),
  dependencies = TRUE,
  update = FALSE,
  force = FALSE
)
```

## Arguments

- pkgs:

  Character vector of package identifiers.

  - For `repo = "cran"` or `"bioc"`: package names, e.g. `"dplyr"`.

  - For `repo = "github"`: `"owner/repo"` strings, e.g.
    `"tidyverse/ggplot2"`.

- repo:

  One of `"cran"`, `"bioc"`, or `"github"`. Default `"cran"`.

- dependencies:

  Logical; forwarded to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  and
  [`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html).
  Default `TRUE`.

- update:

  Logical; only used when `repo = "bioc"`. If `TRUE`, allow
  [`BiocManager::install()`](https://bioconductor.github.io/BiocManager/reference/install.html)
  to update other Bioconductor packages. Default `FALSE` to keep
  installs reproducible.

- force:

  Logical; reinstall even if the package appears installed. (For GitHub,
  the installed check uses the package name part after `/`.)

## Value

Character vector of packages that are still not installed afterward.

## Examples

``` r
if (FALSE) { # \dontrun{
  install_packages(c("dplyr","ggplot2"))                    # CRAN
  install_packages("SummarizedExperiment", repo = "bioc")   # Bioconductor
  install_packages("rstudio/gt", repo = "github")           # GitHub
} # }
```

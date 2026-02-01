# Check which packages are not installed

Check which packages are not installed

## Usage

``` r
check_packages(pkgs)
```

## Arguments

- pkgs:

  Character vector of package names.

## Value

Character vector of packages not currently available.

## Examples

``` r
check_packages(c("stats","definitely_not_a_real_pkg"))
#> [1] "definitely_not_a_real_pkg"
```

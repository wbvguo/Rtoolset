# print p-values in a more readable format

Formats a numeric vector of p-values into a more readable string.
P-values below a threshold are displayed in scientific notation.
Optionally, trailing zeros in the decimal representation can be removed.

## Usage

``` r
print_pval(pvalues, threshold = 1e-04, rm_tailing0 = TRUE)
```

## Arguments

- pvalues:

  A numeric vector of p-values to be formatted.

- threshold:

  The threshold below which p-values are printed in scientific notation.
  Default is 1e-4.

- rm_tailing0:

  Logical. If TRUE, trailing zeros in the decimal representation are
  removed. Default is TRUE.

## Value

A character vector of formatted p-values.

## Examples

``` r
print_pval(c(0.0101, 0.00003, 0.00000001))
#> [1] "0.0101"   "3.00e-05" "1.00e-08"
#[1] "0.0101"   "3.00e-05" "1.00e-08"
print_pval(c(0.0101, 0.00003, 0.00000001), threshold = 1e-3, rm_tailing0 = FALSE)
#> [1] "0.0101"   "3.00e-05" "1.00e-08"
#[1] "0.0101"   "3.00e-05" "1.00e-08"
```

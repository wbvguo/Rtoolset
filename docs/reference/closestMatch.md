# find the closest match of a string in a vector of strings

This function searches the closest match of a string in a vector of
strings using the levenshtein distance, sometimes useful for matching
gene names.

## Usage

``` r
closestMatch(string, stringVector)
```

## Arguments

- string:

  A string

- stringVector:

  A vector of strings to be compared with

## Value

a list of the closest match and its index in the vector

## Examples

``` r
closestMatch("DFNB31", c("DNMT1", "DTNBP1", "IFNB1"))
#> [[1]]
#> [1] "DTNBP1" "IFNB1" 
#> 
#> [[2]]
#> [1] 2 3
#> 
#[[1]] [1] "DTNBP1" "IFNB1"
#[[2]] [1] 2 3
```

# Create a named list

Given a character vector, this function creates an empty list with names
corresponding to the elements of the vector.

## Usage

``` r
createNamedList(vec)
```

## Arguments

- vec:

  A character vector specifying names of each element in the list.

## Value

A named list with the same length as `vec`.

## Examples

``` r
createNamedList(c("x", "y"))
#> $x
#> NULL
#> 
#> $y
#> NULL
#> 
```

# extract and assign default function arguments to the global environment

This function extracts the default arguments of a given function and
assigns them to variables in the global environment. It is particularly
useful for debugging when you need quick access to the default values.

## Usage

``` r
extract_params(func, verbose = FALSE)
```

## Arguments

- func:

  A function

- verbose:

  A logical value. If TRUE, the function will print the default
  parameter values to the console.

## Value

Invisibly returns NULL. Assigns the default values of the function to
the global environment.

## Note

This function modifies the global environment by assigning variables.
This is intentional for debugging purposes but should be used with
caution in production code.

## Examples

``` r
rejection_sampler <- function(D, n_trails = 10000, seed = 2024) {}
extract_params(rejection_sampler)
print(n_trails)
#> [1] 10000
# 10000
```

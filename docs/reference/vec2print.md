# print a vector in a copy-paste friendly format

Formats a vector into a string representation suitable for direct
copy-pasting into code chunks in R, shell scripts, or Python.

## Usage

``` r
vec2print(vec, lang = "R")
```

## Arguments

- vec:

  A vector to be printed.

- lang:

  The language to format the vector for. Options are 'R', 'shell', or
  'python'. Default is 'R'.

## Value

A formatted string representation of the vector.

## Examples

``` r
vec2print(c("Apple", "Orange"))  # For R: c("Apple", "Orange")
#> c("Apple", "Orange")
vec2print(c("Apple", "Orange"), lang = "shell")  # For Shell: ("Apple" "Orange")
#> ("Apple" "Orange")
vec2print(c("Apple", "Orange"), lang = "python")  # For Python: ["Apple", "Orange"]
#> ["Apple", "Orange"]
```

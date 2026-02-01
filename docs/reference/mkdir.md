# create a directory if it does not exist

This function checks whether a directory exists at the specified path.
If the directory does not exist, it is created.

## Usage

``` r
mkdir(dir)
```

## Arguments

- dir:

  A character string specifying the path of the directory to be checked
  and created if necessary.

## Value

The specified directory path, with the directory created if it did not
exist.

## Examples

``` r
mkdir("~/test/")
#> ~/test/ exists! Skip creating...
```

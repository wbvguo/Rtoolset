# Utility Functions Guide

## Overview

This guide covers utility functions organized by category:

- String Matching: Fuzzy string matching for handling typos and
  variations
- Formatting & Printing: Formatting strings for reports and outputs
- File & Directory Management: Safe file operations and directory
  creation
- Function Utilities: Helper functions for debugging and data structures
- Package Management: Unified interface for installing packages from
  multiple sources

## String Matching

### closestMatch()

Find the closest match for a string using Levenshtein distance. Useful
for matching gene names, IDs, or any strings with potential typos.

``` r
library(Rtoolset)

# Find closest match
result <- closestMatch("DFNB31", c("DNMT1", "DTNBP1", "IFNB1"))
result
#> [[1]]
#> [1] "DTNBP1" "IFNB1" 
#> 
#> [[2]]
#> [1] 2 3
```

The function returns a list with:

- Matched strings (may be multiple if tied)

- Indices of matches in the vector

**Use Cases**:

- Matching gene names with typos

- Finding similar identifiers

- Fuzzy string matching

## Formatting & Printing

### print_pval()

Format p-values for readability in reports and publications.

``` r
# Format p-values
print_pval(c(0.0101, 0.00003, 0.00000001))
#> [1] "0.0101"   "3.00e-05" "1.00e-08"
```

**Options**: - `threshold`: P-values below this are shown in scientific
notation (default: 1e-4) - `rm_tailing0`: Remove trailing zeros
(default: TRUE)

``` r
# Custom threshold
print_pval(c(0.0101, 0.00003), threshold = 1e-3)
#> [1] "0.0101"   "3.00e-05"

# Keep trailing zeros
print_pval(c(0.0101, 0.00003), rm_tailing0 = FALSE)
#> [1] "0.0101"   "3.00e-05"
```

### vec2print()

Print vectors in copy-paste friendly format for different languages.

``` r
# R format (default)
vec2print(c("Apple", "Orange", "Banana"))
#> c("Apple", "Orange", "Banana")

# Python format
vec2print(c("Apple", "Orange", "Banana"), lang = "python")
#> ["Apple", "Orange", "Banana"]

# Shell format
vec2print(c("Apple", "Orange", "Banana"), lang = "shell")
#> ("Apple" "Orange" "Banana")
```

**Use Cases**: - Quick code generation - Copying vectors between
languages - Creating configuration files

## File & Directory Management

### mkdir()

Create a directory if it doesn’t exist. Automatically creates parent
directories.

``` r
# Create directory (safe - won't error if exists)
mkdir("~/my_project/results/")

# Nested directories
mkdir("~/project/data/raw/2024/")
```

### save2pdf()

Save plots to PDF with automatic file management and append capability.

``` r
# Save a plot
save2pdf(
  file = "my_plot.pdf",
  width = 6,
  height = 6,
  overwrite = TRUE,
  plot_code = quote({
    plot(1:10, 1:10, main = "My Plot")
  })
)

# Append to existing file
save2pdf(
  file = "my_plot.pdf",
  width = 6,
  height = 6,
  overwrite = FALSE,  # Don't overwrite
  append = TRUE,      # Append instead
  plot_code = quote({
    plot(10:1, 1:10, main = "Second Plot")
  })
)
```

**Features**: - Auto-creates directories - Prevents accidental
overwrites (creates numbered files) - Can append multiple plots to one
PDF - Supports all [`pdf()`](https://rdrr.io/r/grDevices/pdf.html)
arguments (width, height, etc.)

## Function Utilities

### extract_params()

Extract default parameters from functions for debugging.

``` r
# Define a function
my_function <- function(x, n = 100, seed = 2024, verbose = FALSE) {
  # function body
}

# Extract defaults
extract_params(my_function, verbose = TRUE)
```

**Use Cases**: - Quick debugging - Understanding function defaults -
Documentation

### createNamedList()

Create a named list from a character vector.

``` r
# Create named list
my_list <- createNamedList(c("x", "y", "z"))
my_list
#> $x
#> NULL
#> 
#> $y
#> NULL
#> 
#> $z
#> NULL

# Populate it
my_list$x <- 1
my_list$y <- 2
my_list$z <- 3
my_list
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
#> $z
#> [1] 3
```

**Use Cases**: - Initializing lists with known names - Creating
structured data containers

## Package Management

### install_packages()

Install packages from CRAN, Bioconductor, or GitHub with a unified
interface.

``` r
# Install from CRAN
install_packages(c("dplyr", "ggplot2"))

# Install from Bioconductor
install_packages("SummarizedExperiment", repo = "bioc")

# Install from GitHub
install_packages("rstudio/gt", repo = "github")

# With options
install_packages(
  c("dplyr", "ggplot2"),
  dependencies = TRUE,
  force = FALSE
)
```

**Features**: - Skips already installed packages (unless
`force = TRUE`) - Handles dependencies automatically - Works with CRAN,
Bioconductor, and GitHub

### check_packages()

Check if packages are installed.

``` r
# Check packages
check_packages(c("dplyr", "ggplot2", "nonexistent"))
```

Returns a logical vector indicating which packages are installed.

## Use Cases

### 1. Gene Name Matching

When working with gene names that may have typos or variations:

``` r
# Match gene names with potential typos
gene_names <- c("TP53", "BRCA1", "EGFR", "MYC")
query <- "TP5"  # Typo
match_result <- closestMatch(query, gene_names)
match_result
```

### 2. Publication-Ready Tables

Format p-values for manuscripts and reports:

``` r
# Format p-values for a results table
p_values <- c(0.045, 0.0001, 0.0000005, 0.23)
formatted <- print_pval(p_values)
data.frame(
  Gene = c("Gene1", "Gene2", "Gene3", "Gene4"),
  P_value = formatted
)
```

### 3. Multi-Language Code Generation

Generate code snippets for different languages:

``` r
# Generate R code
samples <- c("Sample1", "Sample2", "Sample3")
vec2print(samples, lang = "r")

# Generate Python code
vec2print(samples, lang = "python")
```

## Tips and Best Practices

1.  **String Matching**: Use
    [`closestMatch()`](https://wbvguo.github.io/Rtoolset/reference/closestMatch.md)
    when exact matches might fail due to typos
2.  **P-values**: Always use
    [`print_pval()`](https://wbvguo.github.io/Rtoolset/reference/print_pval.md)
    for publication-ready formatting
3.  **File Management**: Use
    [`mkdir()`](https://wbvguo.github.io/Rtoolset/reference/mkdir.md)
    and
    [`save2pdf()`](https://wbvguo.github.io/Rtoolset/reference/save2pdf.md)
    to avoid file system errors
4.  **Package Installation**: Use
    [`install_packages()`](https://wbvguo.github.io/Rtoolset/reference/install_packages.md)
    for consistent installation across sources
5.  **Error Handling**: Functions like
    [`mkdir()`](https://wbvguo.github.io/Rtoolset/reference/mkdir.md)
    are safe to call multiple times without errors
6.  **Performance**: Caching is enabled for faster vignette rebuilds

## See Also

- Function reference:
  [`?closestMatch`](https://wbvguo.github.io/Rtoolset/reference/closestMatch.md),
  [`?print_pval`](https://wbvguo.github.io/Rtoolset/reference/print_pval.md),
  [`?vec2print`](https://wbvguo.github.io/Rtoolset/reference/vec2print.md),
  [`?mkdir`](https://wbvguo.github.io/Rtoolset/reference/mkdir.md),
  [`?save2pdf`](https://wbvguo.github.io/Rtoolset/reference/save2pdf.md)
- [Full documentation website](https://wbvguo.github.io/Rtoolset/)

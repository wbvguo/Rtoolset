# DNA Methylation

This vignette demonstrates DNA methylation preprocessing using the
[`Rtoolset::beta2M()`](https://wbvguo.github.io/Rtoolset/reference/beta2M.md)
helper.

## Overview

DNA methylation data is often represented as beta values between 0 and
1, while many statistical analyses work better with M values. The
[`beta2M()`](https://wbvguo.github.io/Rtoolset/reference/beta2M.md)
function provides a simple way to convert beta values to M values while
handling edge cases and preserving the input structure.

## Beta to M Value Conversion

### Basic Usage

``` r
library(Rtoolset)

beta2M(0.5)
#> [1] 0

beta_values <- c(0.1, 0.5, 0.9, 0.01, 0.99)
M_values <- beta2M(beta_values)
M_values
#> [1] -3.169912  0.000000  3.169912 -6.629214  6.629214
```

### Matrix or Data Frame Input

``` r
beta_matrix <- matrix(
  runif(100, 0, 1),
  nrow = 10,
  ncol = 10
)
M_matrix <- beta2M(beta_matrix)

beta_df <- data.frame(
  Sample1 = runif(10, 0, 1),
  Sample2 = runif(10, 0, 1),
  Sample3 = runif(10, 0, 1)
)
M_df <- beta2M(beta_df)
```

### Custom `alpha` Parameter

The `alpha` parameter prevents `log(0)` when beta values are exactly 0
or 1:

``` r
beta2M(c(0, 0.5, 1))
#> [1] -19.93157   0.00000  19.93157
beta2M(c(0, 0.5, 1), alpha = 1e-8)
#> [1] -26.57542   0.00000  26.57542
```

## Why Convert to M Values?

M values are often preferred for modeling because they:

1.  are closer to a normal distribution
2.  have more stable variance across the range
3.  work better with linear modeling frameworks

## Example Workflow

``` r
library(Rtoolset)

beta_data <- read.csv("methylation_beta_values.csv", row.names = 1)
M_data <- beta2M(beta_data)
```

## Downstream Analysis

### Differential Methylation with `limma`

``` r
library(limma)

M_data <- beta2M(beta_data)

design <- model.matrix(~ group)
fit <- lmFit(M_data, design)
fit <- eBayes(fit)
top_sites <- topTable(fit, coef = 2, number = 100)
```

### Quality Check for Extreme Beta Values

``` r
extreme_beta <- sum(beta_data == 0 | beta_data == 1, na.rm = TRUE)
cat("Number of extreme beta values:", extreme_beta, "\n")

M_data <- beta2M(beta_data, alpha = 1e-6)
```

## Conversion Formula

The beta-to-M transformation is:

``` math
M = \log_2\left(\frac{\beta + \alpha}{1 - \beta + \alpha}\right)
```

where:

- `beta` is the methylation beta value
- `alpha` is a small constant added for numerical stability
- `M` is the transformed M value

## Best Practices

1.  Check for extreme values before conversion
2.  Use the default `alpha` unless you have a specific reason to change
    it
3.  Record the `alpha` value used for reproducibility
4.  Convert to M values before fitting linear models

## See Also

- [Omics
  Analysis](https://wbvguo.github.io/Rtoolset/articles/omics-overview.md)
- [RNA-seq
  Utilities](https://wbvguo.github.io/Rtoolset/articles/omics-rnaseq.md)

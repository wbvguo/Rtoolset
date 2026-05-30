# Utility Functions for DNA Methylation Analysis

This vignette demonstrates DNA methylation data preprocessing using
Rtoolset functions.

## Overview

DNA methylation data is typically represented as beta values (ranging
from 0 to 1), but many statistical analyses require M values
(logit-transformed beta values) for better statistical properties.

The [`beta2M()`](https://wbvguo.github.io/Rtoolset/reference/beta2M.md)
function in Rtoolset provides a simple and efficient way to convert beta
values to M values, handling edge cases (0 and 1) and preserving data
structure.

## Beta to M Value Conversion

### Basic Usage

The [`beta2M()`](https://wbvguo.github.io/Rtoolset/reference/beta2M.md)
function converts beta values to M values:

``` r
library(Rtoolset)

# Single value
beta2M(0.5)
#> [1] 0

# Vector
beta_values <- c(0.1, 0.5, 0.9, 0.01, 0.99)
M_values <- beta2M(beta_values)
M_values
#> [1] -3.169912  0.000000  3.169912 -6.629214  6.629214
```

### Matrix or Data Frame

``` r
# Matrix input
beta_matrix <- matrix(
  runif(100, 0, 1),
  nrow = 10,
  ncol = 10
)
M_matrix <- beta2M(beta_matrix)

# Data frame input
beta_df <- data.frame(
  Sample1 = runif(10, 0, 1),
  Sample2 = runif(10, 0, 1),
  Sample3 = runif(10, 0, 1)
)
M_df <- beta2M(beta_df)
```

### Custom Alpha Parameter

The `alpha` parameter prevents log(0) when beta values are exactly 0 or
1:

``` r
# Default alpha (1e-6)
beta2M(c(0, 0.5, 1))
#> [1] -19.93157   0.00000  19.93157

# Custom alpha
beta2M(c(0, 0.5, 1), alpha = 1e-8)
#> [1] -26.57542   0.00000  26.57542
```

## Why Convert to M Values?

M values have several advantages for statistical analysis:

1.  **Better distribution**: M values are approximately normally
    distributed
2.  **Improved variance**: Variance is more stable across the range
3.  **Better for linear models**: More suitable for regression and
    differential analysis

## Complete Workflow Example

``` r
library(Rtoolset)

# 1. Load beta values (e.g., from Illumina array)
beta_data <- read.csv("methylation_beta_values.csv", row.names = 1)

# 2. Convert to M values
M_data <- beta2M(beta_data)

# 3. Continue with downstream analysis
# - Differential methylation analysis
# - Clustering
# - Visualization
# - etc.
```

## Use Cases

### 1. Illumina Array Data

Convert beta values from Illumina methylation arrays:

``` r
# Load Illumina array data (beta values)
beta_data <- read.csv("illumina_beta_values.csv", row.names = 1)

# Convert to M values for analysis
M_data <- beta2M(beta_data)

# Check dimensions and structure are preserved
dim(beta_data)
dim(M_data)
colnames(M_data)  # Column names preserved
```

### 2. Differential Methylation Analysis

Prepare data for differential analysis with limma:

``` r
library(limma)

# Convert to M values
M_data <- beta2M(beta_data)

# Create design matrix
design <- model.matrix(~ group)

# Fit linear model
fit <- lmFit(M_data, design)
fit <- eBayes(fit)

# Get top differentially methylated sites
top_sites <- topTable(fit, coef = 2, number = 100)
```

### 3. Quality Control

Check for problematic beta values before conversion:

``` r
# Check for extreme values
extreme_beta <- sum(beta_data == 0 | beta_data == 1, na.rm = TRUE)
cat("Number of extreme beta values (0 or 1):", extreme_beta, "\n")

# Convert with appropriate alpha
M_data <- beta2M(beta_data, alpha = 1e-6)
```

## Best Practices

1.  **Check for extreme values**: Beta values of exactly 0 or 1 may
    indicate technical issues
2.  **Alpha parameter**: Use default (1e-6) unless you have specific
    requirements
3.  **Data structure**: Function preserves matrix/data.frame structure
    and names
4.  **Memory**: For very large datasets, consider processing in chunks
5.  **Reproducibility**: Always document the alpha parameter used in
    your analysis
6.  **Performance**: Caching is enabled for faster vignette rebuilds

## Technical Details

### Conversion Formula

The conversion from beta to M values uses the logit transformation:

$$M = \log_{2}\left( \frac{\beta + \alpha}{1 - \beta + \alpha} \right)$$

where: - $\beta$ is the beta value (0 to 1) - $\alpha$ is a small
constant (default: 1e-6) to prevent log(0) - $M$ is the resulting M
value

### Why M Values?

1.  **Normal Distribution**: M values are approximately normally
    distributed, making them suitable for parametric tests
2.  **Stable Variance**: Variance is more stable across the methylation
    range
3.  **Linear Models**: Better suited for linear regression and
    differential analysis
4.  **Symmetric Scale**: M values range from $-\infty$ to $+\infty$,
    centered around 0

## Integration with Other Packages

M values work well with:

- **limma**: For differential methylation analysis
- **minfi**: For Illumina array preprocessing
- **ChAMP**: For comprehensive methylation analysis
- **DSS**: For differential analysis of bisulfite sequencing data

``` r
# Example with limma
library(limma)

# Convert to M values
M_data <- beta2M(beta_data)

# Design matrix
design <- model.matrix(~ group)

# Fit linear model
fit <- lmFit(M_data, design)
fit <- eBayes(fit)

# Get differentially methylated sites
top_genes <- topTable(fit, coef = 2, number = 100)
```

## See Also

- Function reference:
  [`?beta2M`](https://wbvguo.github.io/Rtoolset/reference/beta2M.md)
- [Full documentation website](https://wbvguo.github.io/Rtoolset/)

# Utility Functions for RNA-seq Analysis

This vignette demonstrates a complete RNA-seq analysis workflow using
Rtoolset functions for filtering, normalization, and preprocessing count
data.

## Overview

Rtoolset provides functions for:

- Filtering low-expression genes
- Calculating counts per million (CPM)
- Log transformation with pseudo counts
- Identifying most variable genes

These functions integrate seamlessly with the `edgeR` package for
differential expression analysis. The
[`filter_calcpm_dge()`](https://wbvguo.github.io/Rtoolset/reference/filter_calcpm_dge.md)
function is a comprehensive workflow function that performs multiple
preprocessing steps in one call, while individual functions like
[`log_transform()`](https://wbvguo.github.io/Rtoolset/reference/log_transform.md)
and
[`get_top_var_mat()`](https://wbvguo.github.io/Rtoolset/reference/get_top_var_mat.md)
provide flexibility for custom workflows.

## Complete Workflow

### Step 1: Load and Prepare Data

``` r
library(Rtoolset)
library(edgeR)

# Your count matrix: genes (rows) x samples (columns)
# count_df should be a data.frame or matrix
count_df <- read.csv("count_matrix.csv", row.names = 1)

# Group information (optional)
group <- c(rep("Control", 3), rep("Treatment", 3))
```

### Step 2: Filter and Normalize

The
[`filter_calcpm_dge()`](https://wbvguo.github.io/Rtoolset/reference/filter_calcpm_dge.md)
function performs multiple steps:

1.  Creates a DGEList object
2.  Calculates CPM
3.  Filters low-expression genes
4.  Normalizes library sizes
5.  Calculates filtered CPM and log-transformed CPM

``` r
result <- filter_calcpm_dge(
  count_df = count_df,
  group = group,
  min_count = 10,      # Minimum count threshold
  min_prop = 0.1       # Minimum proportion of samples
)
```

### Step 3: Access Results

The function returns a list with multiple objects:

``` r
# Original DGEList (all genes)
result$dge_count

# CPM for all genes
result$dge_cpm

# Filtered DGEList (kept genes only)
result$dge_keep

# CPM for filtered genes
result$dge_keep_cpm

# Log2(CPM + 1) for filtered genes (most commonly used)
result$dge_keep_cpm_log
```

### Step 4: Continue with Differential Expression

Use the filtered DGEList for downstream analysis:

``` r
# Estimate dispersions
dge <- estimateDisp(result$dge_keep)

# Fit model
fit <- glmQLFit(dge)

# Test for differential expression
qlf <- glmQLFTest(fit, contrast = c(-1, 1))

# Get results
top_genes <- topTags(qlf, n = 100)
```

## Individual Functions

### Log Transformation

For custom transformations:

``` r
# Log transform with custom parameters
log_data <- log_transform(
  df_mat = count_df,
  count = 1,    # Pseudo count
  base = 2      # Log base
)
```

### Most Variable Genes

Identify genes with highest variance:

``` r
# Get top 20% most variable genes
var_result <- get_top_var_mat(
  count_df = result$dge_keep_cpm_log,
  prop = 0.2
)

# Or get top N genes
var_result <- get_top_var_mat(
  count_df = result$dge_keep_cpm_log,
  topN = 1000
)

# Access results
var_result$var_genes      # Variance for all genes
var_result$topN_mat       # Expression matrix for top genes
```

## Filtering Parameters

### min_count

Minimum count threshold for a gene to be kept:

``` r
# Stricter filtering (keep only highly expressed genes)
result_strict <- filter_calcpm_dge(
  count_df = count_df,
  min_count = 50,  # Higher threshold
  min_prop = 0.1
)

# Lenient filtering (keep more genes)
result_lenient <- filter_calcpm_dge(
  count_df = count_df,
  min_count = 5,   # Lower threshold
  min_prop = 0.1
)
```

### min_prop

Minimum proportion of samples in the smallest group that must have the
minimum count:

``` r
# Require gene to be expressed in at least 50% of smallest group
result <- filter_calcpm_dge(
  count_df = count_df,
  group = group,
  min_count = 10,
  min_prop = 0.5  # More stringent
)
```

## Use Cases

### 1. Standard Differential Expression Analysis

Complete workflow from raw counts to differential expression:

``` r
library(Rtoolset)
library(edgeR)

# Load and preprocess
count_df <- read.csv("counts.csv", row.names = 1)
group <- c(rep("Control", 4), rep("Treatment", 4))

# Filter and normalize
result <- filter_calcpm_dge(
  count_df = count_df,
  group = group,
  min_count = 10,
  min_prop = 0.1
)

# Differential expression
dge <- estimateDisp(result$dge_keep)
fit <- glmQLFit(dge)
qlf <- glmQLFTest(fit, contrast = c(-1, 1))
top_genes <- topTags(qlf, n = 100)
```

### 2. Exploratory Data Analysis

Identify most variable genes for visualization:

``` r
# Get top variable genes
var_result <- get_top_var_mat(
  count_df = result$dge_keep_cpm_log,
  prop = 0.2  # Top 20% most variable
)

# Use for PCA or clustering
pca_data <- var_result$topN_mat
```

### 3. Custom Filtering Workflow

Build a custom preprocessing pipeline:

``` r
# Custom log transformation
log_data <- log_transform(
  df_mat = count_df,
  count = 1,
  base = 2
)

# Manual filtering
keep <- rowSums(count_df >= 10) >= 3
filtered_data <- count_df[keep, ]

# Calculate CPM
dge <- DGEList(counts = filtered_data)
dge <- calcNormFactors(dge)
cpm_data <- cpm(dge, log = TRUE, prior.count = 1)
```

## Best Practices

1.  **Group Information**: Always provide group information when
    available for better filtering
2.  **Filtering Thresholds**:
    - `min_count = 10` and `min_prop = 0.1` are good defaults
    - Adjust based on your data characteristics
    - More stringent filtering (higher thresholds) reduces noise but may
      lose lowly expressed genes
3.  **Normalization**: The function uses edgeR’s
    [`calcNormFactors()`](https://rdrr.io/pkg/edgeR/man/calcNormFactors.html)
    for TMM normalization
4.  **Log Transformation**: Use `dge_keep_cpm_log` for most downstream
    analyses (PCA, clustering, etc.)
5.  **Variable Genes**: Use
    [`get_top_var_mat()`](https://wbvguo.github.io/Rtoolset/reference/get_top_var_mat.md)
    to identify genes for visualization and dimensionality reduction
6.  **Performance**: Caching is enabled for faster vignette rebuilds
7.  **Reproducibility**: Set seeds when using random processes in
    downstream analysis

## Technical Details

### Filtering Strategy

The
[`filter_calcpm_dge()`](https://wbvguo.github.io/Rtoolset/reference/filter_calcpm_dge.md)
function uses edgeR’s
[`filterByExpr()`](https://rdrr.io/pkg/edgeR/man/filterByExpr.html)
function, which:

1.  Calculates the minimum count threshold based on library sizes
2.  Requires genes to have at least `min_count` reads in at least
    `min_prop` proportion of samples in the smallest group
3.  This ensures genes are expressed in a meaningful number of samples

### Normalization

The function uses TMM (Trimmed Mean of M-values) normalization via
[`calcNormFactors()`](https://rdrr.io/pkg/edgeR/man/calcNormFactors.html),
which:

- Accounts for differences in library sizes
- Is robust to outliers
- Is widely used in RNA-seq analysis

### Log Transformation

The log transformation uses:

$$\text{log2(CPM + 1)}$$

where the pseudo-count of 1 prevents log(0) and stabilizes variance for
low counts.

## Example: Complete Analysis

``` r
library(Rtoolset)
library(edgeR)

# 1. Load data
count_df <- read.csv("counts.csv", row.names = 1)
group <- c(rep("Control", 4), rep("Treatment", 4))

# 2. Filter and normalize
result <- filter_calcpm_dge(
  count_df = count_df,
  group = group,
  min_count = 10,
  min_prop = 0.1
)

# 3. Get most variable genes for visualization
var_genes <- get_top_var_mat(
  count_df = result$dge_keep_cpm_log,
  prop = 0.2
)

# 4. Continue with edgeR for differential expression
dge <- estimateDisp(result$dge_keep)
fit <- glmQLFit(dge)
qlf <- glmQLFTest(fit, contrast = c(-1, 1))
top_genes <- topTags(qlf, n = 100)
```

## See Also

- Function reference:
  [`?filter_calcpm_dge`](https://wbvguo.github.io/Rtoolset/reference/filter_calcpm_dge.md),
  [`?log_transform`](https://wbvguo.github.io/Rtoolset/reference/log_transform.md),
  [`?get_top_var_mat`](https://wbvguo.github.io/Rtoolset/reference/get_top_var_mat.md)
- edgeR documentation:
  [`?edgeR::DGEList`](https://rdrr.io/pkg/edgeR/man/DGEList.html),
  [`?edgeR::filterByExpr`](https://rdrr.io/pkg/edgeR/man/filterByExpr.html)
- [Full documentation website](https://wbvguo.github.io/Rtoolset/)

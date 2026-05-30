# RNA-seq

This vignette demonstrates a compact RNA-seq preprocessing workflow
using `Rtoolset` functions for filtering, normalization, and feature
selection.

## Overview

`Rtoolset` provides functions for:

- filtering low-expression genes
- calculating counts per million (CPM)
- log transformation with pseudo counts
- identifying the most variable genes

These functions integrate naturally with the `edgeR` package for
downstream differential expression analysis.

## Complete Workflow

### Step 1: Load and Prepare Data

``` r
library(Rtoolset)
library(edgeR)

# Count matrix: genes (rows) x samples (columns)
count_df <- read.csv("count_matrix.csv", row.names = 1)

# Group labels (optional)
group <- c(rep("Control", 3), rep("Treatment", 3))
```

### Step 2: Filter and Normalize

The
[`filter_calcpm_dge()`](https://wbvguo.github.io/Rtoolset/reference/filter_calcpm_dge.md)
function performs multiple preprocessing steps in one call:

1.  Creates a `DGEList` object
2.  Calculates CPM
3.  Filters low-expression genes
4.  Normalizes library sizes
5.  Computes filtered CPM and log-transformed CPM

``` r
result <- filter_calcpm_dge(
  count_df = count_df,
  group = group,
  min_count = 10,
  min_prop = 0.1
)
```

### Step 3: Access Results

``` r
# Original DGEList
result$dge_count

# CPM for all genes
result$dge_cpm

# Filtered DGEList
result$dge_keep

# CPM for retained genes
result$dge_keep_cpm

# Log2(CPM + 1) matrix for retained genes
result$dge_keep_cpm_log
```

### Step 4: Continue with Differential Expression

``` r
dge <- estimateDisp(result$dge_keep)
fit <- glmQLFit(dge)
qlf <- glmQLFTest(fit, contrast = c(-1, 1))
top_genes <- topTags(qlf, n = 100)
```

## Individual Functions

### Log Transformation

Use
[`log_transform()`](https://wbvguo.github.io/Rtoolset/reference/log_transform.md)
when you want to control the pseudo count or log base:

``` r
log_data <- log_transform(
  df_mat = count_df,
  count = 1,
  base = 2
)
```

### Most Variable Genes

Use
[`get_top_var_mat()`](https://wbvguo.github.io/Rtoolset/reference/get_top_var_mat.md)
to focus on the most variable features for PCA, clustering, or
exploratory analysis:

``` r
var_result <- get_top_var_mat(
  count_df = result$dge_keep_cpm_log,
  prop = 0.2
)

var_result$var_genes
var_result$topN_mat
```

## Filtering Parameters

### `min_count`

The minimum count threshold for a gene to be kept:

``` r
result_strict <- filter_calcpm_dge(
  count_df = count_df,
  min_count = 50,
  min_prop = 0.1
)

result_lenient <- filter_calcpm_dge(
  count_df = count_df,
  min_count = 5,
  min_prop = 0.1
)
```

### `min_prop`

The minimum proportion of samples in the smallest group that must pass
the expression threshold:

``` r
result <- filter_calcpm_dge(
  count_df = count_df,
  group = group,
  min_count = 10,
  min_prop = 0.5
)
```

## Suggested Use Cases

### Standard Differential Expression Analysis

``` r
library(Rtoolset)
library(edgeR)

count_df <- read.csv("counts.csv", row.names = 1)
group <- c(rep("Control", 4), rep("Treatment", 4))

result <- filter_calcpm_dge(
  count_df = count_df,
  group = group,
  min_count = 10,
  min_prop = 0.1
)

dge <- estimateDisp(result$dge_keep)
fit <- glmQLFit(dge)
qlf <- glmQLFTest(fit, contrast = c(-1, 1))
top_genes <- topTags(qlf, n = 100)
```

### Exploratory Data Analysis

``` r
var_result <- get_top_var_mat(
  count_df = result$dge_keep_cpm_log,
  prop = 0.2
)

pca_data <- var_result$topN_mat
```

## See Also

- [Omics
  Analysis](https://wbvguo.github.io/Rtoolset/articles/omics-overview.md)
- [DNA Methylation
  Utilities](https://wbvguo.github.io/Rtoolset/articles/omics-methylation.md)
